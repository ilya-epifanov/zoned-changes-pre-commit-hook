FROM clux/muslrust:1.34.0-stable as builder

WORKDIR /src

ARG TARGET=x86_64-unknown-linux-musl

RUN rustup target add $TARGET
ENV USER=root

RUN cargo init
COPY Cargo.toml Cargo.lock ./
RUN cargo build --target $TARGET --release

COPY . .

RUN rm target/$TARGET/release/deps/zoned_changes_pre_commit_hook*
RUN cargo test --target $TARGET --release
RUN cargo install --target $TARGET --path .

FROM alpine:3.9.3

COPY --from=builder /root/.cargo/bin/zoned-changes-pre-commit-hook /bin/zoned-changes-pre-commit-hook
