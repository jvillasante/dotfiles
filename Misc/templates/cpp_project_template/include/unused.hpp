#ifndef UNUSED_H
#define UNUSED_H

namespace utils {
template <typename... Ts>
inline constexpr void unused(Ts&&... ts) {
    ((void)ts, ...);
}
} // namespace utils

#endif /* UNUSED_H */
