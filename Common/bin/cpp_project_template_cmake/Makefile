CURRENT=app

clean:
	rm -rf bin/*
	rm -rf build/*

all: debug
	bin/app

debug:
	mkdir -p build && cd build && \
  cmake -DCMAKE_BUILD_TYPE=DEBUG .. && make -j4

run_debug: debug
	bin/${CURRENT}

release:
	mkdir -p build && cd build && \
  cmake -DCMAKE_BUILD_TYPE=RELEASE .. && make -j4

run_release: release
	bin/${CURRENT}
