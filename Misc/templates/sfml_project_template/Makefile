CURRENT=Minimal

.PHONY: clean
clean:
	rm -rf bin
	rm -rf build

.PHONY: debug
debug:
	cmake -E make_directory "build" && \
    cmake -S . -B "build" -G Ninja -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1 && \
    cmake --build build --config Debug --target install

.PHONY: run_debug
run_debug: debug
	./bin/$(CURRENT)/$(CURRENT)

.PHONY: release
release:
	cmake -E make_directory "build" && \
    cmake -S . -B "build" -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=1 && \
    cmake --build build --config Release --target install

.PHONY: run_release
run_release: release
	./bin/$(CURRENT)/$(CURRENT)

.DEFAULT_GOAL := release
