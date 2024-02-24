CLEAN   = cargo clean
CHECK   = cargo check
BUILD   = cargo build
CLIPPY  = cargo clippy
RUN     = cargo run
ARGS    =
TEST    = cargo nextest run
DOC     = cargo doc
CURRENT = app

.PHONY: clean
clean:
	${CLEAN}

.PHONY: clean-current
clean-current:
	cd ${CURRENT} && ${CLEAN}

.PHONY: check
check:
	${CHECK}

.PHONY: check-current
check-current:
	cd ${CURRENT} && ${CHECK}

.PHONY: clippy
clippy:
	${CLIPPY}

.PHONY: clippy-current
clippy-current:
	cd ${CURRENT} && ${CLIPPY}

.PHONY: build
build:
	${BUILD}

.PHONY: build-current
build-current:
	cd ${CURRENT} && ${BUILD}

.PHONY: build-release
build-release:
	${BUILD} --release

.PHONY: build-release-current
build-release-current:
	cd ${CURRENT} && ${BUILD} --release

.PHONY: run
run:
	cd ${CURRENT} && ${RUN}

.PHONY: run-release
run-release:
	cd ${CURRENT} && ${RUN} --release

.PHONY: test
test:
	${TEST}

.PHONY: test-current
test-current:
	cd ${CURRENT} && ${TEST}

.PHONY: doc
doc:
	${DOC}

.PHONY: doc-current
doc-current:
	cd ${CURRENT} && ${DOC}

.DEFAULT_GOAL := build
