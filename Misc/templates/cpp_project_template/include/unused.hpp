#pragma once

namespace utils {
template <typename... Ts>
inline constexpr void unused(Ts&&... ts)
{
    ((void)ts, ...);
}
} // namespace utils
