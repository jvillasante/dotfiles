#ifndef BENCHMARKING_H
#define BENCHMARKING_H

#include <chrono>
#include <functional>

namespace utils::benchmarking {
//
// Perf timer implementation to measure functions
//
// Usage:
// auto t = perf_timer<std::chrono::microseconds>::duration(function_to_measure, arg1, arg2, arg3);
//
// auto t = perf_timer<>::duration([]() {
//   for (int i{0}; i < 10000; ++i) {
//     function_to_messure();
//   }
// });
//
// auto nanoseconds = std::chrono::duration<double, std::nano>(t).count();
// auto microseconds = std::chrono::duration<double, std::micro>(t).count();
// auto milliseconds = std::chrono::duration<double, std::milli>(t).count();
// auto seconds = std::chrono::duration<double>(t).count();
// auto minutes = std::chrono::duration<double, std::ratio<60>>(t).count();
// auto hours = std::chrono::duration<double, std::ratio<3600>>(t).count();
//
template <typename Time = std::chrono::microseconds,
          typename Clock = std::chrono::high_resolution_clock>
struct perf_timer {
    template <typename F, typename... Args>
    static Time duration(F&& f, Args&&... args) {
        auto start = Clock::now();

        std::invoke(std::forward<F>(f), std::forward<Args>(args)...);

        auto end = Clock::now();

        return std::chrono::duration_cast<Time>(end - start);
    }
};
} // namespace utils::benchmarking

#endif /* BENCHMARKING_H */
