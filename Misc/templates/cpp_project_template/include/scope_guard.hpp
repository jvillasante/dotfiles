#pragma once
#include <type_traits>
#include <utility>

/**
 * `ScopeGuard` allows you to ensure something gets run at the end of a scope.
 * It also allows you to dismiss the action (with the `dismiss` method) or to
 * rehire a previously dismissed action (with the `rehire` method).
 */

namespace utils
{
class ScopeGuardBase
{
public:
    void dismiss() noexcept { dismissed_ = true; }
    void rehire() noexcept { dismissed_ = false; }

    ScopeGuardBase(ScopeGuardBase const&) = delete;
    ScopeGuardBase(ScopeGuardBase&&) = delete;
    ScopeGuardBase& operator=(ScopeGuardBase const&) = delete;
    ScopeGuardBase& operator=(ScopeGuardBase&&) = delete;

protected:
    bool dismissed_;
    explicit ScopeGuardBase(bool dismissed = false) : dismissed_(dismissed) {}
    ~ScopeGuardBase() = default;
};

// requires: ScopeGuard should store its callable by value
template <typename Func>
    requires(!std::is_reference_v<Func> && !std::is_const_v<Func> &&
             !std::is_volatile_v<Func>)
class ScopeGuard : public ScopeGuardBase
{
public:
    explicit ScopeGuard(Func const& func) noexcept : func_(func) {}
    explicit ScopeGuard(Func&& func) noexcept : func_(std::move(func)) {}
    ScopeGuard(ScopeGuard const&) = delete;
    ScopeGuard(ScopeGuard&&) noexcept = delete;
    ScopeGuard& operator=(ScopeGuard const&) = delete;
    ScopeGuard& operator=(ScopeGuard&&) noexcept = delete;
    ~ScopeGuard() noexcept
    {
        // In the implementation of the destructor we have three options:
        //   1. Require that `func_` is `noexcept` and forget about the issues.
        //   2. Catch any exceptions `func_` can throw and do nothing or log.
        //   3. Catch any exceptions `func_` can throw, log a message and
        //      re-throw allowing the program to fail.
        // In general, 1 is the best option and the one used here.

        // NOTE: if `func_()` throws, `std::terminate()` will be called!
        if (!dismissed_) { func_(); }
    }

private:
    Func func_;
};

template <typename Func>
ScopeGuard<std::remove_cv_t<std::remove_reference_t<Func>>>
make_guard(Func&& func)
{
    return ScopeGuard<std::remove_cv_t<std::remove_reference_t<Func>>>(
        std::forward<Func>(func));
}

namespace detail
{
struct ScopeGuardOnExit
{};
template <typename Func>
ScopeGuard<Func> operator+(ScopeGuardOnExit /*unused*/, Func&& func)
{
    return utils::make_guard(std::forward<Func>(func));
}
} // namespace detail

// These macros will let us ignore the result when using `ScopeGuard` since we
// are not interested in the result itself (C++ demands that we name it) but on
// the destructor being run even in the face of failure. Notice that, even when
// these macros are namespaced, they are pre-processor macros and as such they
// will be added to the global namespace. Usage example:
//
// 1. auto _ = utils::make_guard([&]() {
//        on_scope_variable.method();
//        std::cout << "this is the end" << '\n';
//    });
//    The action will always run at the end of the current scope. The caller can
//    capture variables on the lambda. Unfortunately we need to name the return
//    type with the "___" macro and forgetting is a bug that the compiler won't
//    catch!
//
// 2. ON_SCOPE_EXIT {
//        on_scope_variable.method();
//        std::cout << "this is the end" << '\n';
//    };
//    The action will always run at the end of the current scope. The macro
//    captures all variables by reference. No need to name the return value, it
//    is done implicitly.
//
// 3. ON_SCOPE_EXIT_NAMED(variable_name) {
//        on_scoped_variable.method();
//        std::cout << "this is the end";
//    };
//    The action will always run at the end of the current scope unless it is
//    dismissed before the scope ends by using the `variable_name` provided by
//    the caller as `variable_name.dismiss()`, which is also the return value
//    name of the entire expression. The macro captures all variables by
//    reference.
#define CONCAT2(x, y) x##y
#define CONCAT(x, y) CONCAT2(x, y)
#ifdef __COUNTER__
#define _ CONCAT(dont_care, __COUNTER__)
#define ANON_VAR(x) CONCAT(x, __COUNTER__)
#else
#define ___ CONCAT(dont_care, __LINE__)
#define ANON_VAR(x) CONCAT(x, __LINE__)
#endif

#define ON_SCOPE_EXIT                                                          \
    auto ANON_VAR(SCOPE_EXIT_STATE) =                                          \
        utils::detail::ScopeGuardOnExit() + [&]() noexcept

#define ON_SCOPE_EXIT_NAMED(NAME)                                              \
    auto NAME = utils::detail::ScopeGuardOnExit() + [&]() noexcept
} // namespace utils
