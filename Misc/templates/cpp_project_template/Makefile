CURRENT=app

clean:
	rm -rf bin/*
	rm -rf build/*

all: debug
	bin/app

debug:
	cmake -E make_directory "build" && \
    cmake -E chdir "build" cmake -DCMAKE_BUILD_TYPE=Debug ../ && \
    cmake --build "build" --config Release

run_debug: debug
	bin/${CURRENT}

release:
	cmake -E make_directory "build" && \
    cmake -E chdir "build" cmake -DCMAKE_BUILD_TYPE=Release ../ && \
    cmake --build "build" --config Release

run_release: release
	bin/${CURRENT}
