#pragma once
#include <array>
#include <cstdint>
#include <random>

namespace utils::math
{
template <typename input_t = std::uint32_t>
input_t get_bit(input_t num, input_t pos)
{
    return (num >> pos) & 1;
}

template <typename input_t = std::uint32_t>
input_t set_bit(input_t num, input_t pos, bool value)
{
    return value ? (num | (1 << pos)) : (num & ~(1 << pos));
}

template <typename input_t = std::uint32_t>
input_t set_bit_range(input_t num, input_t start, input_t end, input_t value)
{
    input_t mask = ((1U << (end - start + 1)) - 1) << start;
    num &= ~mask;                   // Clear the bit range just in case it has old values
    num |= (value << start) & mask; // Set the bit range to the desired value
    return num;
}

template <typename input_t = std::uint32_t>
input_t get_bit_range(input_t num, input_t start, input_t end)
{
    input_t mask = ((1U << (end - start + 1)) - 1) << start;
    return (num & mask) >> start;
}

inline int random_int(int min, int max)
{
    // One engine instance per thread
    auto thread_local static engine = std::default_random_engine{std::random_device{}()};
    return std::uniform_int_distribution<>{min, max}(engine);
}

template <std::uint8_t N, typename input_t = std::uint32_t, typename sum_t = std::uint64_t>
class simple_moving_average
{
public:
    input_t operator()(input_t const input)
    {
        sum_ -= previous_inputs_[index_];
        sum_ += input;
        previous_inputs_[index_] = input;
        if (++index_ == N) { index_ = 0; }
        return (sum_ + (N / 2)) / N;
    }

private:
    std::uint8_t index_ = 0;
    std::array<input_t, N> previous_inputs_ = {0};
    sum_t sum_ = 0;
};
} // namespace utils::math
