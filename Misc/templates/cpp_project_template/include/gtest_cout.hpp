#pragma once
#include <sstream>

#define PRINTF(...)                                                                                                                \
    do {                                                                                                                           \
        printf("%s", "[      OUT>] ");                                                                                             \
        printf(__VA_ARGS__);                                                                                                       \
    } while (0)

// C++ stream interface
class gtest_cout : public std::stringstream {
public:
    gtest_cout() = default;
    gtest_cout(gtest_cout const&) = delete;
    gtest_cout(gtest_cout&&) = delete;
    gtest_cout& operator=(gtest_cout const&) = delete;
    gtest_cout& operator=(gtest_cout&&) = delete;
    ~gtest_cout() override { PRINTF("%s", str().c_str()); } // NOLINT
};

#define GCOUT gtest_cout()
