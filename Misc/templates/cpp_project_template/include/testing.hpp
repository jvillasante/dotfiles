#pragma once
#include <iostream>
#include <sstream>
#include <source_location>
#include <unordered_map>

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
struct StatsCounter
{
    enum class Stats : std::uint8_t
    {
        DefaultConstructor,
        Constructor,
        CopyConstructor,
        MoveConstructor,
        Destructor,
        CopyAssignment,
        MoveAssignment,
    };

    static void clear_stats()
    {
        for (auto& [_, value] : stats_)
        {
            value = 0;
        }
    }
    static void print_stats(char const* header, std::ostream& os = std::cout)
    {
        os << "----- " << header << '\n';
        os << "default constructor calls = " << stats_[Stats::DefaultConstructor] << '\n';
        os << "        constructor calls = " << stats_[Stats::Constructor] << '\n';
        os << "   copy constructor calls = " << stats_[Stats::CopyConstructor] << '\n';
        os << "   move constructor calls = " << stats_[Stats::MoveConstructor] << '\n';
        os << "         destructor calls = " << stats_[Stats::Destructor] << '\n';
        os << "    copy assignment calls = " << stats_[Stats::CopyAssignment] << '\n';
        os << "    move assignment calls = " << stats_[Stats::MoveAssignment] << '\n';
    }
    static void increment_stat(Stats const key) { stats_.at(key)++; }
    static std::size_t get_stat(Stats const key) { return stats_.at(key); }

private:
    // NOLINTNEXTLINE
    inline static std::unordered_map<Stats, std::size_t> stats_ = {
        {Stats::DefaultConstructor, 0}, {Stats::Constructor, 0}, {Stats::CopyConstructor, 0},
        {Stats::MoveConstructor, 0},    {Stats::Destructor, 0},  {Stats::CopyAssignment, 0},
        {Stats::MoveAssignment, 0},
    };
};
template <typename T, bool Print = false>
class Lifetime : public StatsCounter
{
public:
    Lifetime() noexcept : value_()
    {
        increment_stat(Stats::DefaultConstructor);
        if constexpr (Print)
        {
            std::cout << std::source_location::current().function_name() //
                      << "\n\t self=" << value_ << " @ " << this         //
                      << '\n';
        }
    }
    Lifetime(T value) noexcept : value_(value) // NOLINT
    {
        increment_stat(Stats::Constructor);
        if constexpr (Print)
        {
            std::cout << std::source_location::current().function_name() //
                      << "\n\t self=" << value_ << " @ " << this         //
                      << '\n';
        }
    }
    Lifetime(Lifetime const& rhs) noexcept : value_(rhs.value_)
    {
        increment_stat(Stats::CopyConstructor);
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
        increment_stat(Stats::MoveConstructor);
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
        increment_stat(Stats::Destructor);
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
        increment_stat(Stats::CopyAssignment);
        if constexpr (Print)
        {
            std::cout << std::source_location::current().function_name() //
                      << "\n\t  rhs=" << rhs.value_ << " @ " << &rhs     //
                      << "\n\t self=" << value_ << " @ " << this         //
                      << '\n';
        }

        return *this;
    }
    Lifetime& operator=(Lifetime&& rhs) noexcept // NOLINT (don't care about self assignment)
    {
        value_ = std::move(rhs.value_);
        increment_stat(Stats::MoveAssignment);
        if constexpr (Print)
        {
            std::cout << std::source_location::current().function_name() //
                      << "\n\t  rhs=" << rhs.value_ << " @ " << &rhs     //
                      << "\n\t self=" << value_ << " @ " << this         //
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
