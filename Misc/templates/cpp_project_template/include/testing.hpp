#pragma once
#include <iostream>
#include <sstream>
#include <utility>
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
        MemberSwap,
        NonMemberSwap,
        ObjectCount,
        ObjectTotalCount,
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
        os << "        member swap calls = " << stats_[Stats::MemberSwap] << '\n';
        os << "    non member swap calls = " << stats_[Stats::NonMemberSwap] << '\n';
        os << "             object count = " << stats_[Stats::ObjectCount] << '\n';
        os << "       object total count = " << stats_[Stats::ObjectTotalCount] << '\n';
    }

    static void increment_stat(Stats const key) { stats_.at(key)++; }
    static void increment_stats(std::initializer_list<Stats const> il)
    {
        for (Stats const key : il)
        {
            increment_stat(key);
        }
    }
    static void decrement_stat(Stats const key) { stats_.at(key)--; }
    static void decrement_stats(std::initializer_list<Stats const> il)
    {
        for (Stats const key : il)
        {
            decrement_stat(key);
        }
    }
    static std::size_t get_stat(Stats const key) { return stats_.at(key); }

private:
    // NOLINTNEXTLINE
    inline static std::unordered_map<Stats, std::size_t> stats_ = {
        {Stats::DefaultConstructor, 0}, {Stats::Constructor, 0},      {Stats::CopyConstructor, 0},
        {Stats::MoveConstructor, 0},    {Stats::Destructor, 0},       {Stats::CopyAssignment, 0},
        {Stats::MoveAssignment, 0},     {Stats::MemberSwap, 0},       {Stats::NonMemberSwap, 0},
        {Stats::ObjectCount, 0},        {Stats::ObjectTotalCount, 0},
    };
};
template <typename T, bool Print = false>
class Lifetime : public StatsCounter
{
public:
    Lifetime() noexcept : value_()
    {
        increment_stats({Stats::DefaultConstructor, Stats::ObjectCount, Stats::ObjectTotalCount});
        if constexpr (Print) { print(std::source_location::current(), this); }
    }
    Lifetime(T value) noexcept : value_(value) // NOLINT (explicit-conversion)
    {
        increment_stats({Stats::Constructor, Stats::ObjectCount, Stats::ObjectTotalCount});
        if constexpr (Print) { print(std::source_location::current(), this); }
    }
    Lifetime(Lifetime const& rhs) noexcept : value_(rhs.value_)
    {
        increment_stats({Stats::CopyConstructor, Stats::ObjectCount, Stats::ObjectTotalCount});
        if constexpr (Print) { print(std::source_location::current(), this, &rhs); }
    }
    Lifetime(Lifetime&& rhs) noexcept : value_(std::move(rhs.value_))
    {
        increment_stats({Stats::MoveConstructor, Stats::ObjectCount, Stats::ObjectTotalCount});
        if constexpr (Print) { print(std::source_location::current(), this, &rhs); }
    }
    ~Lifetime() noexcept
    {
        increment_stat(Stats::Destructor);
        decrement_stat(Stats::ObjectCount);
        if constexpr (Print) { print(std::source_location::current(), this); }
    }
    Lifetime& operator=(Lifetime const& rhs) noexcept // NOLINT (don't care about self assignment)
    {
        value_ = rhs.value_;
        increment_stat(Stats::CopyAssignment);
        if constexpr (Print) { print(std::source_location::current(), this, &rhs); }

        return *this;
    }
    Lifetime& operator=(Lifetime&& rhs) noexcept // NOLINT (don't care about self assignment)
    {
        value_ = std::move(rhs.value_);
        increment_stat(Stats::MoveAssignment);
        if constexpr (Print) { print(std::source_location::current(), this, &rhs); }

        return *this;
    }

    [[nodiscard]] T value() const { return value_; }
    void swap(Lifetime& rhs) noexcept
    {
        increment_stat(Stats::MemberSwap);

        using std::swap;
        swap(value_, rhs.value_);
    }
    friend void swap(Lifetime& lhs, Lifetime& rhs) noexcept
    {
        increment_stat(Stats::NonMemberSwap);
        lhs.swap(rhs);
    }
    friend bool operator==(Lifetime const& lhs, Lifetime const& rhs) { return lhs.value_ == rhs.value_; }
    friend bool operator!=(Lifetime const& lhs, Lifetime const& rhs) { return !(lhs == rhs); }
    friend std::ostream& operator<<(std::ostream& out, Lifetime const& lp)
    {
        out << lp.value_;
        return out;
    }

private:
    T value_;

    void print(std::source_location const& loc, Lifetime const* self = nullptr, Lifetime const* rhs = nullptr) const
    {
        std::cout << loc.function_name();
        if (self != nullptr) { std::cout << "\n\t self=" << self->value_ << " @ " << self; }
        if (rhs != nullptr) { std::cout << "\n\t  rhs=" << rhs->value_ << " @ " << rhs; }
        std::cout << '\n';
    }
};
} // namespace utils::testing
