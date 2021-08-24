CURRENT=Minimal

clean:
	rm -rf bin/*
	rm -rf build/*

all: release

debug:
	cmake -E make_directory "build" && \
    cmake -E chdir "build" cmake -DCMAKE_BUILD_TYPE=Debug ../ && \
    cmake --build "build" --target install --config Debug

run_debug: debug
	./bin/$(CURRENT)/$(CURRENT).app/Contents/MacOS/$(CURRENT)

release:
	cmake -E make_directory "build" && \
    cmake -E chdir "build" cmake -DCMAKE_BUILD_TYPE=Release ../ && \
    cmake --build "build" --target install --config Release

run_release: release
	./bin/$(CURRENT)/$(CURRENT).app/Contents/MacOS/$(CURRENT)
