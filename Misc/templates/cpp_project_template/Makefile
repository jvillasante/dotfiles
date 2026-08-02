# --- Configuration ---
# Directories to search for formatting and linting
FORMAT_DIRS := include src test tools

# Default target when you just run `make`
.DEFAULT_GOAL := help

# --- Help Target ---

# Shows this help menu
.PHONY: help
help:
	@printf "Usage: make \033[36m[target]\033[0m\n\n"
	@echo "Available targets:"
	@awk '/^# / { desc = substr($$0, 3) } \
		/^[a-zA-Z0-9_-]+:/ && desc { \
			sub(/:/, "", $$1); \
			printf "  \033[36m%-15s\033[0m %s\n", $$1, desc; \
			desc = "" \
		}' $(MAKEFILE_LIST)
	@echo ""

# --- Build & Clean Targets ---

# Keeps CMake configuration, deletes binaries/objects
.PHONY: clean
clean:
	for dir in debug release relwithdebinfo minsizerel sanitize tsan; do \
		if test -d "build/$$dir"; then \
			cmake --build "build/$$dir" --target clean; \
		fi; \
	done

# Wipes out the entire build folder to start completely from scratch
.PHONY: distclean
distclean:
	@echo "Nuking all build directories..."
	@rm -rf build/

# # Configures and builds in Debug mode with ASan and UBSan enabled
# .PHONY: sanitize
# sanitize:
# 	cmake -S . -B "build/sanitize" -G Ninja \
# 		-DCMAKE_BUILD_TYPE=Debug \
# 		-DENABLE_SANITIZERS=ON \
# 		-DCMAKE_EXPORT_COMPILE_COMMANDS=1
# 	cmake --build build/sanitize
# 	@ln -sf build/sanitize/compile_commands.json build/compile_commands.json

# Configures and builds in Debug mode with ASan and UBSan enabled
.PHONY: sanitize
sanitize:
	cmake -S . -B "build/sanitize" -G Ninja \
		-DCMAKE_BUILD_TYPE=Debug \
		-DENABLE_ASAN=ON \
		-DCMAKE_EXPORT_COMPILE_COMMANDS=1
	cmake --build build/sanitize
	@ln -sf build/sanitize/compile_commands.json build/compile_commands.json

# Configures and builds in Debug mode with ThreadSanitizer (TSan) enabled
.PHONY: tsan
tsan:
	cmake -S . -B "build/tsan" -G Ninja \
		-DCMAKE_BUILD_TYPE=Debug \
		-DENABLE_TSAN=ON \
		-DCMAKE_EXPORT_COMPILE_COMMANDS=1
	cmake --build build/tsan
	@ln -sf build/tsan/compile_commands.json build/compile_commands.json

# Configures and builds the project in Debug mode, plus links compile_commands
.PHONY: debug
debug:
	cmake -S . -B "build/debug" -G Ninja \
		-DCMAKE_BUILD_TYPE=Debug \
		-DCMAKE_EXPORT_COMPILE_COMMANDS=1
	cmake --build build/debug
	@ln -sf build/debug/compile_commands.json build/compile_commands.json

# Configures and builds the project in Release mode (tests disabled)
.PHONY: release
release:
	cmake -S . -B "build/release" -G Ninja \
		-DCMAKE_BUILD_TYPE=Release \
		-DBUILD_TESTING=OFF
	cmake --build build/release

# Configures and builds in Release mode with debug symbols included
.PHONY: relwithdebinfo
relwithdebinfo:
	cmake -S . -B "build/relwithdebinfo" -G Ninja \
		-DCMAKE_BUILD_TYPE=RelWithDebInfo \
		-DBUILD_TESTING=OFF
	cmake --build build/relwithdebinfo

# Configures and builds in Release mode optimized for binary size
.PHONY: minsizerel
minsizerel:
	cmake -S . -B "build/minsizerel" -G Ninja \
		-DCMAKE_BUILD_TYPE=MinSizeRel \
		-DBUILD_TESTING=OFF
	cmake --build build/minsizerel

# --- Testing & Installation ---

# Builds the debug target first, then runs the CTest test suite
.PHONY: test
test: debug
	ctest --output-on-failure --test-dir build/debug

# Alias for 'test'
.PHONY: tests
tests: test

# Builds with sanitizers, then runs the CTest test suite
.PHONY: test-sanitize
test-sanitize: sanitize
	ctest --output-on-failure --test-dir build/sanitize

# Alias for 'test-sanitize'
.PHONY: tests-sanitize
tests-sanitize: test-sanitize

# Builds with ThreadSanitizer, then runs the CTest test suite
.PHONY: test-tsan
test-tsan: tsan
	ctest --output-on-failure --test-dir build/tsan

# Alias for 'test-tsan'
.PHONY: tests-tsan
tests-tsan: test-tsan

# Builds the release target first, then installs the artifacts
.PHONY: install
install: release
	cmake --install build/release

# --- Tooling Targets ---

# Formats all C++ files in FORMAT_DIRS using clang-format
.PHONY: format
format:
	@echo "Formatting C++ files..."
	@find $(FORMAT_DIRS) -type f \
		\( -name "*.cpp" -o -name "*.hpp" \
		-o -name "*.h" -o -name "*.cc" \) \
		-exec clang-format -style=file -i {} +
	@echo "Formatting complete!"

# Formats only the specific lines you've modified in Git before committing
.PHONY: format-patch
format-patch:
	git clang-format

# Runs clang-tidy on all source files
.PHONY: tidy
tidy: debug
	@echo "Running clang-tidy..."
	@find $(FORMAT_DIRS) -type f \( -name "*.cpp" -o -name "*.cc" \) \
		-exec clang-tidy -p build/debug --header-filter=".*" {} +
	@echo "Clang-tidy complete!"

# Runs clang-tidy and automatically applies suggested fixes
.PHONY: tidy-fix
tidy-fix: debug
	@echo "Running clang-tidy and applying fixes..."
	@find $(FORMAT_DIRS) -type f \( -name "*.cpp" -o -name "*.cc" \) \
		-exec clang-tidy -p build/debug --header-filter=".*" --fix {} +
	@echo "Clang-tidy fix complete!"
