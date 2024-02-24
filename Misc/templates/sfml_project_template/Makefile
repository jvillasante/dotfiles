clean:
	rm -rf bin
	rm -rf build

all: debug

debug:
	cmake -E make_directory "build" && \
    cmake -E chdir "build" cmake -DCMAKE_BUILD_TYPE=Debug -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ../ && \
    cmake --build "build" --target install --config Debug

release:
	cmake -E make_directory "build" && \
    cmake -E chdir "build" cmake -DCMAKE_BUILD_TYPE=Release -DCMAKE_EXPORT_COMPILE_COMMANDS=1 ../ && \
    cmake --build "build" --target install --config Release
