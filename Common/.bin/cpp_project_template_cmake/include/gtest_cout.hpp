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
class test_gcout : public std::stringstream {
public:
    test_gcout() = default;
    test_gcout(test_gcout const&) = delete;
    test_gcout(test_gcout&&) = delete;
    test_gcout& operator=(test_gcout const&) = delete;
    test_gcout& operator=(test_gcout&&) = delete;
    ~test_gcout() override { PRINTF("%s", str().c_str()); }
};

#define GCOUT test_gcout()

#endif /* GTEST_COUT_H */
