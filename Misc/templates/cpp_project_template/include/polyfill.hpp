#pragma once

/**
 * Define `std::make_unique` if the compiler is C++11, but not C++14 or greater.
 * This implementation is the one proposed by Stephan T. Lavavej in
 * N3656: https://isocpp.org/files/papers/N3656.txt.
 */

#if __cplusplus == 201103L
#include <cstddef>
#include <memory>
#include <type_traits>
#include <utility>

// It's bad to add things to the std namespace, but the point is to "polyfill"
// this function for C++11 compilers.
namespace std
{
template <typename T>
struct _Unique_if
{
    typedef unique_ptr<T> _Single_object;
};

template <typename T>
struct _Unique_if<T[]>
{
    typedef unique_ptr<T[]> _Unknown_bound;
};

template <typename T, std::size_t N>
struct _Unique_if<T[N]>
{
    typedef void _Known_bound;
};

template <typename T, typename... Args>
typename _Unique_if<T>::_Single_object make_unique(Args&&... args)
{
    return unique_ptr<T>(new T(std::forward<Args>(args)...));
}

template <typename T>
typename _Unique_if<T>::_Unknown_bound make_unique(std::size_t n)
{
    typedef typename remove_extent<T>::type U;
    return unique_ptr<T>(new U[n]());
}

template <typename T, typename... Args>
typename _Unique_if<T>::_Known_bound make_unique(Args&&...) = delete;
} // namespace std
#endif // __cplusplus == 201103L

/**
 * Define `std::exchange` if the compiler is C++11, but not C++14 or greater.
 */

#if __cplusplus == 201103L
#include <type_traits>
#include <utility>

// It's bad to add things to the std namespace, but the point is to "polyfill"
// this function for C++11 compilers.
namespace std
{
template <typename T, typename U = T>
T exchange(T& obj, U&& new_value) noexcept(
    std::is_nothrow_move_constructible<T>::value &&
    std::is_nothrow_assignable<T&, U>::value)
{
    T old_value = std::move(obj);
    obj = std::forward<U>(new_value);
    return old_value;
}
} // namespace std
#endif // __cplusplus == 201103L
