CLEAN   = cargo clean
CHECK   = cargo check
BUILD   = cargo build
CLIPPY  = cargo clippy
RUN     = cargo run
ARGS    =
TEST    = cargo test
DOC     = cargo doc
CURRENT = ch02/guessing_game

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

test:
	${TEST}

test-current:
	cd ${CURRENT} && ${TEST}

doc:
	${DOC}

doc-current:
	cd ${CURRENT} && ${DOC}
