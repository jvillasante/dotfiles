#include <gtest/gtest.h>
#include <testing.hpp>

// Demonstrate some basic assertions.
TEST(HelloTest, BasicAssertions)
{
    // Expect two strings not to be equal.
    EXPECT_STRNE("hello", "world");

    // Expect equality.
    EXPECT_EQ(7 * 6, 42);

    // Print through Cpp Utils
    GCOUT << "Hello World" << '\n';
}
