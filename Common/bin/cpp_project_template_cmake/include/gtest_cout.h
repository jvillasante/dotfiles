#ifndef GTEST_COUT_H
#define GTEST_COUT_H

#include "gmock/gmock.h"
#include "gtest/gtest.h"

namespace testing {
namespace internal {
// enum GTestColor { COLOR_DEFAULT, COLOR_RED, COLOR_GREEN, COLOR_YELLOW };

extern void ColoredPrintf(GTestColor color, const char* fmt, ...);
} // namespace internal
} // namespace testing
#define PRINTF(...)                                                                                \
    do {                                                                                           \
        testing::internal::ColoredPrintf(testing::internal::COLOR_GREEN, "[      OUT>] ");         \
        testing::internal::ColoredPrintf(testing::internal::COLOR_YELLOW, __VA_ARGS__);            \
    } while (0)

// C++ stream interface
class TestCout : public std::stringstream {
public:
    ~TestCout() { PRINTF("%s", str().c_str()); }
};

#define GCOUT TestCout()

#endif /* GTEST_COUT_H */
