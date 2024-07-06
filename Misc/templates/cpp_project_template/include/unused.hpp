#pragma once

namespace utils
{
template <typename... Ts>
inline constexpr void unused(Ts&&... ts) // NOLINT
{
    ((void)ts, ...);
}
} // namespace utils
