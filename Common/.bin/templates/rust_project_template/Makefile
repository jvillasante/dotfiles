CLEAN   = cargo clean
CHECK   = cargo check
BUILD   = cargo build
CLIPPY  = cargo clippy
RUN     = cargo run
TEST    = cargo test
DOC     = cargo doc
CURRENT = src/app
ALL     = src/app

clean_all:
	$(foreach project,$(ALL),cd $(project) && ${CLEAN} && cd -;)

clean:
	cd ${CURRENT} && ${CLEAN}

check_all:
	$(foreach project,$(ALL),cd $(project) && ${CHECK} && cd -;)

check:
	cd ${CURRENT} && ${CHECK}

clippy_all:
	$(foreach project,$(ALL),cd $(project) && ${CLIPPY} && cd -;)

clippy:
	cd ${CURRENT} && ${CLIPPY}

build_all:
	$(foreach project,$(ALL),cd $(project) && ${BUILD} && cd -;)

build:
	cd ${CURRENT} && ${BUILD}

build_all_release:
	$(foreach project,$(ALL),cd $(project) && ${BUILD} --release && cd -;)

build_release:
	cd ${CURRENT} && ${BUILD} --release

run:
	cd ${CURRENT} && ${RUN}

run_release:
	cd ${CURRENT} && ${RUN} --release

test_all:
	$(foreach project,$(ALL),cd $(project) && ${TEST} && cd -;)

test:
	cd ${CURRENT} && ${TEST}

doc_all:
	$(foreach project,$(ALL),cd $(project) && ${DOC} && cd -;)

doc:
	cd ${CURRENT} && ${DOC}
