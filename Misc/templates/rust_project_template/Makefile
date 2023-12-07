CLEAN   = cargo clean
CHECK   = cargo check
BUILD   = cargo build
CLIPPY  = cargo clippy
RUN     = cargo run
ARGS    =
TEST    = cargo nextest run
DOC     = cargo doc
CURRENT = app

clean:
	${CLEAN}

clean-current:
	cd ${CURRENT} && ${CLEAN}

check:
	${CHECK}

check-current:
	cd ${CURRENT} && ${CHECK}

clippy:
	${CLIPPY}

clippy-current:
	cd ${CURRENT} && ${CLIPPY}

build:
	${BUILD}

build-current:
	cd ${CURRENT} && ${BUILD}

build-release:
	${BUILD} --release

build-release-current:
	cd ${CURRENT} && ${BUILD} --release

run:
	cd ${CURRENT} && ${RUN}

run-release:
	cd ${CURRENT} && ${RUN} --release

test:
	${TEST}

test-current:
	cd ${CURRENT} && ${TEST}

doc:
	${DOC}

doc-current:
	cd ${CURRENT} && ${DOC}
