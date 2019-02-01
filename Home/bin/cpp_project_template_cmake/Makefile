clean:
	rm -rf bin/*
	rm -rf build/*

debug:
	mkdir -p build && cd build && \
  cmake -DCMAKE_BUILD_TYPE=DEBUG .. && make -j4

release:
	mkdir -p build && cd build && \
  cmake -DCMAKE_BUILD_TYPE=RELEASE .. && make -j4
