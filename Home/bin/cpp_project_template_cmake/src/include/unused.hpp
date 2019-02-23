#ifndef UNUSED_H
#define UNUSED_H

template <typename... Ts>
constexpr void unused(Ts&&... ts) {
  ((void)ts, ...);
}

#endif /* UNUSED_H */
