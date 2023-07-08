#pragma once
#include <memory>
#include <algorithm>

namespace utils::smart_pointers {
/**
 * @brief Converts std::unique_ptr of base type to std::unique_ptr of derived type
 * by using static_cast internally
 * @details Ownership of the object is transfered to the returned std::unique_ptr.
 * It is somewhat analogous to std::static_ptr_cast
 */
template <typename Derived, typename Base, typename Deleter>
std::unique_ptr<Derived, Deleter> static_ptr_cast(std::unique_ptr<Base, Deleter> base)
{
    auto deleter = base.get_deleter();
    auto derived_ptr = static_cast<Derived*>(base.release());
    return std::unique_ptr<Derived, Deleter>(derived_ptr, std::move(deleter));
}

/**
 * @details A separate static_ptr_cast() version for std::default_delete<T> is
 * required to provide expected results in use-cases like
 *
 * std::unique_ptr<Base> base(new Derived());
 * auto derived = static_ptr_cast<Derived>(std::move(base));
 *
 * It is desirable for `derived` to have type `std::unique_ptr<Derived>`,
 * but if general version of static_ptr_cast() is used, it will actually
 * be `std::unique_ptr<Derived, std::default_delete<Base>>` which is very
 * inconvenient
 */
template <typename Derived, typename Base>
std::unique_ptr<Derived> static_ptr_cast(std::unique_ptr<Base> base) noexcept
{
    auto derived_ptr = static_cast<Derived*>(base.release());
    return std::unique_ptr<Derived>(derived_ptr);
}

/**
 * @brief Converts std::unique_ptr of base type to std::unique_ptr of derived type
 * by using dynamic_cast internally
 * @details Ownership of the object will be transfered to the returned std::unique_ptr
 * if and only if the dynamic_cast from Base to Derived is successful. Otherwise
 * the input std::unique_ptr will continue to be the owner of the object
 * It is somewhat analogous to std::dynamic_ptr_cast
 */
template <typename Derived, typename Base, typename Deleter>
std::unique_ptr<Derived, Deleter> dynamic_ptr_cast(std::unique_ptr<Base, Deleter>&& base)
{
    if (auto derived = dynamic_cast<Derived*>(base.get())) {
        auto deleter = base.get_deleter();
        base.release();
        return std::unique_ptr<Derived, Deleter>(derived, std::move(deleter));
    }

    return nullptr;
}

/**
 * @details see description to @see static_ptr_cast() version that works only
 * with `std::default_delete<T>` for explanation why this special version of
 * dynamic_ptr_cast() is required
 */
template <typename Derived, typename Base>
std::unique_ptr<Derived> dynamic_ptr_cast(std::unique_ptr<Base>&& base) noexcept
{
    if (auto derived = dynamic_cast<Derived*>(base.get())) {
        base.release();
        return std::unique_ptr<Derived>(derived);
    }

    return nullptr;
}
} // namespace utils::smart_pointers
