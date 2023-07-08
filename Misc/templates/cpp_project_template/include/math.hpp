#pragma once
#include <array>
#include <cstdint>

namespace utils::math {
template <std::uint8_t N, typename input_t = std::uint32_t, typename sum_t = std::uint64_t>
class simple_moving_average {
public:
    input_t operator()(input_t const input)
    {
        sum_ -= previous_inputs_[index_];
        sum_ += input;
        previous_inputs_[index_] = input;
        if (++index_ == N) {
            index_ = 0;
        }
        return (sum_ + (N / 2)) / N;
    }

private:
    std::uint8_t index_ = 0;
    std::array<input_t, N> previous_inputs_ = {0};
    sum_t sum_ = 0;
};
} // namespace utils::math
