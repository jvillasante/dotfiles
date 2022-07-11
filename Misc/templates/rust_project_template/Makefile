CLEAN   = cargo clean
CHECK   = cargo check
BUILD   = cargo build
CLIPPY  = cargo clippy
RUN     = cargo run
ARGS    =
TEST    = cargo test
DOC     = cargo doc
CURRENT = app

clean_all:
	${CLEAN} --workspace

clean:
	cd ${CURRENT} && ${CLEAN}

check_all:
	${CHECK} --workspace

check:
	cd ${CURRENT} && ${CHECK}

clippy_all:
	${CLIPPY} --workspace

clippy:
	cd ${CURRENT} && ${CLIPPY}

build_all:
	${BUILD} --workspace

build:
	cd ${CURRENT} && ${BUILD}

build_all_release:
	${BUILD} --workspace --release

build_release:
	cd ${CURRENT} && ${BUILD} --release

run:
	cd ${CURRENT} && ${RUN} ${ARGS}

run_release:
	cd ${CURRENT} && ${RUN} --release ${ARGS}

test_all:
	${TEST} --workspace

test:
	cd ${CURRENT} && ${TEST}

doc_all:
	${DOC} --workspace

doc:
	cd ${CURRENT} && ${DOC}
