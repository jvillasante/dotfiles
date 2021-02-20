#ifndef FUNCTIONAL_H
#define FUNCTIONAL_H

#include <map>
#include <queue>
#include <numeric>

namespace utils::functional {
namespace mutable_version {
template <typename F, typename R>
inline void mapf(F&& f, R& r) {
    std::transform(std::begin(r), std::end(r), std::begin(r), std::forward<F>(f));
}
} // namespace mutable_version

namespace variadic_function_template_version {
template <typename F, typename T1, typename T2>
inline auto foldl(F&& f, T1 arg1, T2 arg2) {
    return f(arg1, arg2);
}

template <typename F, typename T, typename... Ts>
inline auto foldl(F&& f, T&& head, Ts... rest) {
    return f(head, foldl(std::forward<F>(f), rest...));
}
} // namespace variadic_function_template_version

template <typename F, typename R>
inline R mapf(F&& f, R r) {
    std::transform(std::begin(r), std::end(r), std::begin(r), std::forward<F>(f));
    return r;
}

template <typename F, typename T, typename U>
inline std::map<T, U> mapf(F&& f, const std::map<T, U>& m) {
    std::map<T, U> r;
    for (auto&& kvp : m) {
        // r.insert(f(kvp));
        r.insert(f(std::forward<decltype(kvp)>(kvp)));
    }

    return r;
}

template <typename F, typename T>
inline std::queue<T> mapf(F&& f, std::queue<T> q) {
    std::queue<T> r;

    while (!q.empty()) {
        // r.push(f(q.front()));
        r.push(f(std::forward<T>(q.front())));
        q.pop();
    }

    return r;
}

template <typename F, typename R, typename T>
inline constexpr T foldl(F&& f, R&& r, T i) {
    return std::accumulate(std::begin(r), std::end(r), std::move(i), std::forward<F>(f));
}

template <typename F, typename R, typename T>
inline constexpr T foldr(F&& f, R&& r, T i) {
    // return std::accumulate(std::rbegin(r), std::rend(r), std::move(i), std::forward<F>(f));
    return std::accumulate(
        std::rbegin(r), std::rend(r), std::move(i), [f](auto&& arg1, auto&& arg2) {
            return f(std::forward<decltype(arg2)>(arg2), std::forward<decltype(arg1)>(arg1));
        });
}

template <typename F, typename T>
inline constexpr T foldl(F&& f, std::queue<T> q, T i) {
    while (!q.empty()) {
        // i = f(i, q.front());
        i = f(std::forward<T>(i), std::forward<T>(q.front()));
        q.pop();
    }

    return i;
}

template <typename F, typename G>
inline auto compose(F&& f, G&& g) {
    return [=](auto x) { return f(g(x)); };
}

template <typename F, typename... R>
inline auto compose(F&& f, R&&... r) {
    return [=](auto x) { return f(compose(r...)(x)); };
}
} // namespace utils::functional

#endif /* FUNCTIONAL_H */
