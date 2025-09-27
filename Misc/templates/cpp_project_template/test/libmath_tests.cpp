#include <catch2/catch_test_macros.hpp>
#include <libmath/libmath.hpp>

TEST_CASE("validate environment") { REQUIRE(math::sum(1, 2) == 3); }
