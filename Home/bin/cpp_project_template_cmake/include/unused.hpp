#ifndef UNUSED_H
#define UNUSED_H

namespace unused {
template <typename... Ts>
constexpr void unused(Ts&&... ts) {
    ((void)ts, ...);
}
} // namespace unused

#endif /* UNUSED_H */
