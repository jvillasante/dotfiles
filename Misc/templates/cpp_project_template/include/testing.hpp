#pragma once
#include <cstdio>
#include <cstddef>
#include <iostream>
#include <source_location>

namespace utils::testing
{
inline void print(const std::source_location& location = std::source_location::current()) noexcept
{
    std::puts(location.function_name());
}

struct LifetimePrint
{
    LifetimePrint() noexcept { print(); }
    LifetimePrint(int value) noexcept : value_{value} { print(); } // NOLINT (implicit conversion from int)
    LifetimePrint(const LifetimePrint& /*unused*/) noexcept { print(); }
    LifetimePrint(LifetimePrint&& /*unused*/) noexcept { print(); }
    ~LifetimePrint() noexcept { print(); }
    LifetimePrint& operator=(const LifetimePrint& /*unused*/) noexcept // NOLINT (don't care about self assignment)
    {
        print();
        return *this;
    }
    LifetimePrint& operator=(LifetimePrint&& /*unused*/) noexcept // NOLINT (don't care about self assignment)
    {
        print();
        return *this;
    }

    friend bool operator==(LifetimePrint const& lhs, LifetimePrint const& rhs) { return lhs.value_ == rhs.value_; }
    int value() const { return value_; }

private:
    int value_ = 0;
};

struct LifetimeStats
{
    static inline std::size_t default_constructor_calls = 0;      // NOLINT
    static inline std::size_t int_constructor_calls = 0;          // NOLINT
    static inline std::size_t copy_constructor_calls = 0;         // NOLINT
    static inline std::size_t move_constructor_calls = 0;         // NOLINT
    static inline std::size_t destructor_calls = 0;               // NOLINT
    static inline std::size_t copy_assignment_operator_calls = 0; // NOLINT
    static inline std::size_t move_assignment_operator_calls = 0; // NOLINT
    static inline void print_stats(char const* header, std::ostream& os = std::cout)
    {
        os << "----- " << header << '\n';
        os << "      default_constructor_calls = " << default_constructor_calls << '\n';
        os << "          int_constructor_calls = " << int_constructor_calls << '\n';
        os << "        copy_constructor_calls  = " << copy_constructor_calls << '\n';
        os << "        move_constructor_calls  = " << move_constructor_calls << '\n';
        os << "              destructor_calls  = " << destructor_calls << '\n';
        os << "copy_assignment_operator_calls  = " << copy_assignment_operator_calls << '\n';
        os << "move_assignment_operator_calls  = " << move_assignment_operator_calls << '\n';
    }
    static inline void clear_stats()
    {
        default_constructor_calls = int_constructor_calls = move_constructor_calls = copy_constructor_calls =
            destructor_calls = copy_assignment_operator_calls = move_assignment_operator_calls = 0;
    }

    LifetimeStats() noexcept { ++default_constructor_calls; }
    LifetimeStats(int value) noexcept : value_{value} // NOLINT (implicit conversion from int)
    {
        ++int_constructor_calls;
    }
    LifetimeStats(LifetimeStats const& /*unused*/) noexcept { ++copy_constructor_calls; }
    LifetimeStats(LifetimeStats&& /*unused*/) noexcept { ++move_constructor_calls; }
    ~LifetimeStats() noexcept { ++destructor_calls; }
    LifetimeStats& operator=(const LifetimeStats& /*unused*/) noexcept // NOLINT (don't care about self assignment)
    {
        ++copy_assignment_operator_calls;
        return *this;
    }
    LifetimeStats& operator=(LifetimeStats&& /*unused*/) noexcept // NOLINT (don't care about self assignment)
    {
        ++move_assignment_operator_calls;
        return *this;
    }

    friend bool operator==(LifetimeStats const& lhs, LifetimeStats const& rhs) { return lhs.value_ == rhs.value_; }
    int value() const { return value_; }

private:
    int value_ = 0;
};
} // namespace utils::testing
