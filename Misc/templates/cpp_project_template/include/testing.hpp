#pragma once
#include <iostream>
#include <sstream>
#include <source_location>

namespace utils::testing
{
//
// Declaration only for `TypeDisplayer`, no definition!
// Usage: utils::testing::TypeDisplayer<decltype(x)> xType;
//
template <typename T>
class TypeDisplayer;

//
// Google Test better std::cout
//
#define PRINTF(...)                                                                                                    \
    do                                                                                                                 \
    {                                                                                                                  \
        printf("%s", "[      OUT>] ");                                                                                 \
        printf(__VA_ARGS__);                                                                                           \
    }                                                                                                                  \
    while (0)

// C++ stream interface
class gtest_cout : public std::stringstream
{
public:
    gtest_cout() = default;
    gtest_cout(gtest_cout const&) = delete;
    gtest_cout(gtest_cout&&) = delete;
    gtest_cout& operator=(gtest_cout const&) = delete;
    gtest_cout& operator=(gtest_cout&&) = delete;
    ~gtest_cout() override { PRINTF("%s", str().c_str()); } // NOLINT
};

// `GCOUT` is in the global namespace!
#define GCOUT utils::testing::gtest_cout()

//
// Lifetime Statistics
//
template <typename T, bool Print = false>
struct Lifetime
{
    static inline std::size_t default_constructor_calls = 0;
    static inline std::size_t value_constructor_calls = 0;
    static inline std::size_t copy_constructor_calls = 0;
    static inline std::size_t move_constructor_calls = 0;
    static inline std::size_t destructor_calls = 0;
    static inline std::size_t copy_assignment_operator_calls = 0;
    static inline std::size_t move_assignment_operator_calls = 0;
    static void print_stats(char const* header, std::ostream& os = std::cout)
    {
        os << "----- " << header << '\n';
        os << "      default_constructor_calls = " << default_constructor_calls << '\n';
        os << "        value_constructor_calls = " << value_constructor_calls << '\n';
        os << "        copy_constructor_calls  = " << copy_constructor_calls << '\n';
        os << "        move_constructor_calls  = " << move_constructor_calls << '\n';
        os << "              destructor_calls  = " << destructor_calls << '\n';
        os << "copy_assignment_operator_calls  = " << copy_assignment_operator_calls << '\n';
        os << "move_assignment_operator_calls  = " << move_assignment_operator_calls << '\n';
    }
    static void clear_stats()
    {
        default_constructor_calls = value_constructor_calls = move_constructor_calls = copy_constructor_calls =
            destructor_calls = copy_assignment_operator_calls = move_assignment_operator_calls = 0;
    }

    Lifetime() noexcept : value_()
    {
        ++default_constructor_calls;
        if constexpr (Print)
        {
            std::cout << std::source_location::current().function_name() //
                      << "\n\t self=" << value_ << " @ " << this         //
                      << '\n';
        }
    }
    Lifetime(T value) noexcept : value_(value) // NOLINT
    {
        ++value_constructor_calls;
        if constexpr (Print)
        {
            std::cout << std::source_location::current().function_name() //
                      << "\n\t self=" << value_ << " @ " << this         //
                      << '\n';
        }
    }
    Lifetime(Lifetime const& rhs) noexcept : value_(rhs.value_)
    {
        ++copy_constructor_calls;
        if constexpr (Print)
        {
            std::cout << std::source_location::current().function_name() //
                      << "\n\t  rhs=" << rhs.value_ << " @ " << &rhs     //
                      << "\n\t self=" << value_ << " @ " << this         //
                      << '\n';
        }
    }
    Lifetime(Lifetime&& rhs) noexcept : value_(std::move(rhs.value_))
    {
        ++move_constructor_calls;
        if constexpr (Print)
        {
            std::cout << std::source_location::current().function_name() //
                      << "\n\t  rhs=" << rhs.value_ << " @ " << &rhs     //
                      << "\n\t self=" << value_ << " @ " << this         //
                      << '\n';
        }
    }
    ~Lifetime() noexcept
    {
        ++destructor_calls;
        if constexpr (Print)
        {
            std::cout << std::source_location::current().function_name() //
                      << "\n\t self=" << value_ << " @ " << this         //
                      << '\n';
        }
    }
    Lifetime& operator=(Lifetime const& rhs) noexcept // NOLINT (don't care about self assignment)
    {
        value_ = rhs.value_;
        ++copy_assignment_operator_calls;
        if constexpr (Print)
        {
            std::cout << std::source_location::current().function_name()     //
                      << "\n\t  rhs=" << rhs.value_ << " @ " << &rhs << '\n' //
                      << "\n\t self=" << value_ << " @ " << this             //
                      << '\n';
        }

        return *this;
    }
    Lifetime& operator=(Lifetime&& rhs) noexcept // NOLINT (don't care about self assignment)
    {
        value_ = std::move(rhs.value_);
        ++move_assignment_operator_calls;
        if constexpr (Print)
        {
            std::cout << std::source_location::current().function_name()     //
                      << "\n\t  rhs=" << rhs.value_ << " @ " << &rhs << '\n' //
                      << "\n\t self=" << value_ << " @ " << this             //
                      << '\n';
        }

        return *this;
    }

    friend bool operator==(Lifetime const& lhs, Lifetime const& rhs) { return lhs.value_ == rhs.value_; }
    [[nodiscard]] T value() const { return value_; }
    friend std::ostream& operator<<(std::ostream& out, Lifetime const& lp)
    {
        out << lp.value_;
        return out;
    }

private:
    T value_;
};
} // namespace utils::testing
