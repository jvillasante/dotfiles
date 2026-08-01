.PHONY: clean
clean:
	if test -d "build/debug"; then cmake --build "build/debug" --target clean; fi
	if test -d "build/release"; then cmake --build "build/release" --target clean; fi
	if test -d "build/relwithdebinfo"; then cmake --build "build/relwithdebinfo" --target clean; fi
	if test -d "build/minsizerel"; then cmake --build "build/minsizerel" --target clean; fi

.PHONY: debug
debug:
	cmake -S . -B "build/debug" -G Ninja \
		-DCMAKE_BUILD_TYPE=Debug \
		-DCMAKE_EXPORT_COMPILE_COMMANDS=1
	cmake --build build/debug

.PHONY: release
release:
	cmake -S . -B "build/release" -G Ninja \
		-DCMAKE_BUILD_TYPE=Release \
		-DBUILD_TESTING=OFF
	cmake --build build/release

.PHONY: relwithdebinfo
relwithdebinfo:
	cmake -S . -B "build/relwithdebinfo" -G Ninja \
		-DCMAKE_BUILD_TYPE=RelWithDebInfo \
		-DBUILD_TESTING=OFF
	cmake --build build/relwithdebinfo

.PHONY: minsizerel
minsizerel:
	cmake -S . -B "build/minsizerel" -G Ninja \
		-DCMAKE_BUILD_TYPE=MinSizeRel \
		-DBUILD_TESTING=OFF
	cmake --build build/minsizerel

.PHONY: test
test: debug
	ctest --output-on-failure --test-dir build/debug

.PHONY: install
install: release
	cmake --install build/release

.DEFAULT_GOAL := debug
