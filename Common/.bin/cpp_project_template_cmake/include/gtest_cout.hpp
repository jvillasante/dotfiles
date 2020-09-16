#ifndef GTEST_COUT_H
#define GTEST_COUT_H

#include <sstream>

// Unfortunately this approach doesn't work any longer with modern versions of Google Test
// testing::internal::ColoredPrintf isn't available any longer to the public since it has been
// made static.
//
// namespace testing::internal {
// enum GTestColor { COLOR_DEFAULT, COLOR_RED, COLOR_GREEN, COLOR_YELLOW };
// extern void ColoredPrintf(GTestColor color, const char* fmt, ...);
// } // namespace testing::internal
//
// #define PRINTF(...)                                                                                \
//     do {                                                                                           \
//         testing::internal::ColoredPrintf(testing::internal::COLOR_GREEN, "[      OUT>] ");         \
//         testing::internal::ColoredPrintf(testing::internal::COLOR_YELLOW, __VA_ARGS__);            \
//     } while (0)

#define PRINTF(...)                                                                                \
    do {                                                                                           \
        printf("%s", "[      OUT>] ");                                                             \
        printf(__VA_ARGS__);                                                                       \
    } while (0)

// C++ stream interface
class gtest_cout : public std::stringstream {
public:
    gtest_cout() = default;
    gtest_cout(gtest_cout const&) = delete;
    gtest_cout(gtest_cout&&) = delete;
    gtest_cout& operator=(gtest_cout const&) = delete;
    gtest_cout& operator=(gtest_cout&&) = delete;
    ~gtest_cout() override { PRINTF("%s", str().c_str()); }
};

#define GCOUT gtest_cout()

#endif /* GTEST_COUT_H */
