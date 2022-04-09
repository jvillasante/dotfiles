CURRENT=app

clean:
	rm -rf bin/*
	rm -rf build/*

debug:
	cmake -E make_directory "build" && \
    cmake -S . -B "build" -G Ninja -DCMAKE_BUILD_TYPE=Debug && \
    cmake --build build --config Release

run_debug: debug
	bin/${CURRENT}

release:
	cmake -E make_directory "build" && \
    cmake -S . -B "build" -G Ninja -DCMAKE_BUILD_TYPE=Release && \
    cmake --build build --config Release

run_release: release
	bin/${CURRENT}
