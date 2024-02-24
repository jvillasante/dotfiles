.PHONY: clean
clean:
	rm -rf bin
	rm -rf build

.PHONY: debug
debug:
	cmake -E make_directory "build" && \
    cmake -S . -B "build" -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1 && \
    cmake --build build --config Debug

.PHONY: release
release:
	cmake -E make_directory "build" && \
    cmake -S . -B "build" -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=1 && \
    cmake --build build --config Release

.DEFAULT_GOAL := debug
