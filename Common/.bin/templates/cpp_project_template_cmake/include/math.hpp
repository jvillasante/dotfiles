#ifndef MATH_H
#define MATH_H

#include <cstdint>

namespace utils::math {
template <std::uint8_t N, typename input_t = std::uint32_t,
          typename sum_t = std::uint64_t>
class simple_moving_average {
public:
    input_t operator()(input_t input) {
        sum -= previous_inputs[index];
        sum += input;
        previous_inputs[index] = input;
        if (++index == N)
            index = 0;
        return (sum + (N / 2)) / N;
    }

private:
    uint8_t index = 0;
    input_t previous_inputs[N] = {0};
    sum_t sum = 0;
};
} // namespace utils::math

#endif /* MATH_H */
