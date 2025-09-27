.PHONY: clean
clean:
	if test -d "build/debug"; then cmake --build "build/debug" --target clean; fi
	if test -d "build/release"; then cmake --build "build/release" --target clean; fi
	if test -d "build/relwithdebinfo"; then cmake --build "build/relwithdebinfo" --target clean; fi
	if test -d "build/minsizerel"; then cmake --build "build/minsizerel" --target clean; fi

.PHONY: debug
debug:
	cmake -E make_directory "build/debug" && \
    cmake -S . -B "build/debug" -G Ninja \
          -DCMAKE_BUILD_TYPE=Debug \
          -DCMAKE_EXPORT_COMPILE_COMMANDS=1 && \
    cmake --build build/debug --config Debug

.PHONY: release
release:
	cmake -E make_directory "build/release" && \
    cmake -S . -B "build/release" -G Ninja \
        -DCMAKE_BUILD_TYPE=Release \
        -DBUILD_TESTING=OFF && \
    cmake --build build/release --config Release

.PHONY: relwithdebinfo
relwithdebinfo:
	cmake -E make_directory "build/relwithdebinfo" && \
    cmake -S . -B "build/relwithdebinfo" -G Ninja \
        -DCMAKE_BUILD_TYPE= RelWithDebInfo \
        -DBUILD_TESTING=OFF && \
    cmake --build build/relwithdebinfo --config Release

.PHONY: minsizerel
minsizerel:
	cmake -E make_directory "build/minsizerel" && \
    cmake -S . -B "build/minsizerel" -G Ninja \
        -DCMAKE_BUILD_TYPE= MinSizeRel \
        -DBUILD_TESTING=OFF && \
    cmake --build build/minsizerel --config Release

.PHONY: test
test: debug
	ctest --output-on-failure --test-dir build/debug/test

.PHONY: install
install:
	cmake --build build/release --config Release --target install

.DEFAULT_GOAL := debug
