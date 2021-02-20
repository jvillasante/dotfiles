#ifndef THREADING_H
#define THREADING_H

#include <iostream>
#include <sstream>
#include <mutex>

namespace utils::threading {
struct pcout : public std::stringstream {
    static inline std::mutex cout_mutex; // NOLINT

    pcout() = default;
    pcout(pcout const&) = delete;
    pcout(pcout&&) = delete;
    pcout& operator=(pcout const&) = delete;
    pcout& operator=(pcout&&) = delete;
    ~pcout() override {
        std::lock_guard<std::mutex> l{cout_mutex};
        std::cout << rdbuf();
        std::cout.flush();
    }
};
} // namespace utils::threading

#endif /* THREADING_H */
