CURRENT=src/app
CLEAN=cargo clean
CHECK=cargo check
BUILD=cargo build
CLIPPY=cargo clippy
RUN=cargo run
TEST=cargo test
DOC=cargo doc

clean_all:
	rm -rf src/app/target

clean:
	cd ${CURRENT} && ${CLEAN}

check:
	cd ${CURRENT} && ${CHECK}

clippy:
	cd ${CURRENT} && ${CLIPPY}

build:
	cd ${CURRENT} && ${BUILD}

build_release:
	cd ${CURRENT} && ${BUILD} --release

run:
	cd ${CURRENT} && ${RUN}

run_release:
	cd ${CURRENT} && ${RUN} --release

test:
	cd ${CURRENT} && ${TEST}

doc:
	cd ${CURRENT} && ${DOC}
