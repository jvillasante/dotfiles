#pragma once
#include <map>
#include <queue>
#include <numeric>
#include <functional>

namespace utils::functional {
template <typename F, typename R>
inline R mapf(F&& f, R r)
{
    std::transform(std::begin(r), std::end(r), std::begin(r), std::forward<F>(f));
    return r;
}

template <typename F, typename T, typename U>
inline std::map<T, U> mapf(F&& f, const std::map<T, U>& m)
{
    std::map<T, U> r;
    for (auto&& kvp : m) {
        // r.insert(f(kvp));
        r.insert(f(std::forward<decltype(kvp)>(kvp)));
    }

    return r;
}

template <typename F, typename T>
inline std::queue<T> mapf(F&& f, std::queue<T> q)
{
    std::queue<T> r;

    while (!q.empty()) {
        // r.push(f(q.front()));
        r.push(f(std::forward<T>(q.front())));
        q.pop();
    }

    return r;
}

template <typename F, typename R, typename T>
inline constexpr T foldl(F&& f, R&& r, T i)
{
    return std::accumulate(std::begin(r), std::end(r), std::move(i), std::forward<F>(f));
}

template <typename F, typename R, typename T>
inline constexpr T foldr(F&& f, R&& r, T i)
{
    // return std::accumulate(std::rbegin(r), std::rend(r), std::move(i), std::forward<F>(f));
    return std::accumulate(std::rbegin(r), std::rend(r), std::move(i), [f](auto&& arg1, auto&& arg2) {
        return f(std::forward<decltype(arg2)>(arg2), std::forward<decltype(arg1)>(arg1));
    });
}

template <typename F, typename T>
inline constexpr T foldl(F&& f, std::queue<T> q, T i)
{
    while (!q.empty()) {
        // i = f(i, q.front());
        i = f(std::forward<T>(i), std::forward<T>(q.front()));
        q.pop();
    }

    return i;
}

template <typename T>
auto map(T const fn)
{
    return
        [=](auto const reduce_fn) { return [=](auto accumulator, auto const input) { return reduce_fn(accumulator, fn(input)); }; };
}

template <typename T>
auto filter(T const predicate)
{
    return [=](auto const reduce_fn) {
        return [=](auto accumulator, auto const input) { return predicate(input) ? reduce_fn(accumulator, input) : accumulator; };
    };
}

template <typename T, typename... Ts>
auto concat(T t, Ts... ts)
{
    if constexpr (sizeof...(ts) > 0) {
        return [=](auto... parameters) { return t(concat(ts...)(parameters...)); };
    }
    else {
        return t;
    }
}

template <typename A, typename B, typename F>
auto combine(F binary_func, A a, B b)
{
    return [=](auto const param) { return binary_func(a(param), b(param)); };
}
} // namespace utils::functional
