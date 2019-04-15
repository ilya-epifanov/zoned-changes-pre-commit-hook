#[macro_use]
extern crate log;

use std::path::{Path, Component};

use clap::{App, Arg};
use failure::Error;
use log::LevelFilter;
use simplelog::{TermLogger, Config, SimpleLogger};
use git2::{Repository, DiffOptions, Commit, Oid};
use std::collections::HashMap;
use std::str::FromStr;
use std::fmt::Display;

type Result<T> = std::result::Result<T, Error>;

#[derive(Debug)]
struct GlobPattern {
    pattern: glob::Pattern,
    capture: Option<usize>,
    exclusion: bool,
}

impl FromStr for GlobPattern {
    type Err = Error;

    fn from_str(mut s: &str) -> Result<Self> {
        let exclusion = if s.starts_with('!') {
            s = &s[1..];
            true
        } else {
            false
        };

        let capture = s.find("(**)").map(|capture_ix| {
            (&s[..capture_ix]).chars().filter(|c| *c == '/').count()
        });

        let pattern = glob::Pattern::new(&s.replace("(**)", "**"))?;

        Ok(GlobPattern {
            pattern, capture, exclusion
        })
    }
}

#[derive(Debug)]
struct Group {
    definition: String,
    globs: Vec<GlobPattern>,
}

#[derive(Eq, PartialEq, Hash, Debug)]
struct GroupMatchKey {
    group: String,
    subgroup: Option<String>,
}

impl Display for GroupMatchKey {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        if let Some(subgroup) = &self.subgroup {
            f.write_fmt(format_args!("{}:{}", self.group, subgroup))
        } else {
            f.write_str(&self.group)
        }
    }
}

impl Group {
    fn try_match(&self, path: &Path) -> Option<GroupMatchKey> {
        for glob in self.globs.iter() {
            if glob.pattern.matches_path(path) {
                return if glob.exclusion {
                    None
                } else {
                    Some(GroupMatchKey {
                        group: self.definition.clone(),
                        subgroup: glob.capture.map(|path_ix| {
                            let components: Vec<Component> = path.components().collect();
                            components[path_ix].as_os_str().to_string_lossy().into_owned()
                        })
                    })
                }
            }
        }
        None
    }
}

fn parse_group(group: &str) -> Result<Group> {
    let globs: Result<Vec<GlobPattern>> = group.split(',').map(GlobPattern::from_str).collect();
    Ok(Group { definition: group.to_owned(), globs: globs? })
}

fn n_trailing_ones(n: usize) -> usize {
    match usize::max_value().overflowing_shr((std::mem::size_of::<usize>() * 8 - n) as u32) {
        (v, false) => v,
        (_, true) => 0
    }
}

fn common_ancestor<'repo>(roots: &[Commit<'repo>]) -> Result<Commit<'repo>> {
    assert!(roots.len() <= std::mem::size_of::<usize>());
    let mut commits = Vec::from(roots);

    let mut known_shas = HashMap::<Oid, usize>::new();
    let full_mask = n_trailing_ones(roots.len());

    loop {
        for (ix, root) in commits.iter_mut().enumerate() {
            if root.parent_ids().len() != 0 {
                let id = root.id();
                let root_mask = 1 << ix;
                if let Some(reachable_from_roots) = known_shas.get_mut(&id) {
                    assert_eq!(*reachable_from_roots & root_mask, 0);
                    *reachable_from_roots |= root_mask;
                    if *reachable_from_roots == full_mask {
                        return Ok(root.clone());
                    }
                } else {
                    known_shas.insert(id, root_mask);
                }

                *root = root.parent(0)?;
            }
        }
    }
}

fn main() -> Result<()> {
    let matches = App::new("split-areas")
        .version(env!("CARGO_PKG_VERSION"))
        .about("capitalize files in-place")
        .arg(Arg::with_name("mainline-branch")
            .short("b")
            .long("mainline-branch")
            .help("revision against branch-off point of which to compare the set of current changes")
            .takes_value(true)
            .default_value("HEAD")
        )
        .arg(Arg::with_name("groups")
            .short("g")
            .long("group")
            .help("group fileglob or many fileglobs, comma delimited, representing a group of files to ")
            .multiple(true)
            .number_of_values(1)
            .takes_value(true)
        )
        .arg(Arg::with_name("FILES")
            .index(1)
            .multiple(true)
            .help("path(s) to files to check")
        )
        .arg(Arg::with_name("verbosity")
            .short("v")
            .long("verbosity")
            .multiple(true)
        )
        .get_matches();

    let logging_level = match matches.occurrences_of("verbosity") {
        0 => LevelFilter::Info,
        1 => LevelFilter::Debug,
        _ => LevelFilter::Trace,
    };
    SimpleLogger::init(logging_level, Config::default())?;

    let mainline_branch = matches.value_of("mainline-branch").unwrap();

    let mut groups: Vec<Group> = matches.values_of("groups")
        .map(|gs| {
            let groups: Result<Vec<Group>> = gs.map(parse_group).collect();
            groups
        })
        .unwrap_or_else(|| Ok(Vec::new()))?;

    let repo = Repository::open_from_env()?;

    let mainline_commit = repo.revparse_single(mainline_branch)?.peel_to_commit()?;
    let branch_off_point = common_ancestor(&[mainline_commit, repo.head()?.peel_to_commit()?])?;

    let tree = branch_off_point.tree()?;

    let diff = repo.diff_tree_to_index(Some(&tree), None, Some(DiffOptions::new().show_untracked_content(true)))?;

    let mut hits: HashMap<GroupMatchKey, u32> = HashMap::new();

    for delta in diff.deltas() {
        let new_path = delta.new_file().path().unwrap();

        for g in groups.iter_mut() {
            if let Some(k) = g.try_match(new_path) {
                debug!("File {:?} matched group {}: {}", new_path, g.definition, &k);
                *hits.entry(k).or_default() += 1;
                break;
            }
        }
    }

    if hits.len() > 1 {
        println!("Files from conflicting groups were modified since {}", branch_off_point.id());
        for (k, count) in hits.iter() {
            println!("{:7}\t{}", count, k);
        }
        std::process::exit(1);
    }

    Ok(())
}
