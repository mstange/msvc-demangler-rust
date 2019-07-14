all: check test
.PHONY: all

check-source:
	@cargo check
.PHONY: check-source

check-format:
	@rustup component add rustfmt --toolchain stable 2> /dev/null
	@cargo +stable fmt -- --check
.PHONY: check-format

check: check-source check-format
.PHONY: check

test-cargo:
	@cargo test
.PHONY: test-cargo

test-wasm-build:
	@rustup target add wasm32-unknown-unknown --toolchain stable 2> /dev/null
	@cargo build --target=wasm32-unknown-unknown
.PHONY: test-cargo

test: test-cargo test-wasm-build
.PHONY: test

lint:
	@rustup component add clippy --toolchain stable 2> /dev/null
	@cargo +stable clippy --all --tests -- -D clippy::all
.PHONY: lint

update-readme:
	@cargo-readme -V &> /dev/null || cargo install cargo-readme
	@cargo readme > README.md
.PHONY: update-readme
