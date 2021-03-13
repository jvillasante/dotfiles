#ifndef THREADING_H
#define THREADING_H

#include <iostream>
#include <sstream>
#include <mutex>

namespace utils::threading {
struct pcout : public std::stringstream {
    pcout() = default;
    pcout(pcout const&) = delete;
    pcout(pcout&&) = delete;
    pcout& operator=(pcout const&) = delete;
    pcout& operator=(pcout&&) = delete;
    ~pcout() override {
        std::lock_guard<std::mutex> _{mtx_};
        std::cout << rdbuf();
        std::cout.flush();
    }

private:
    static inline std::mutex mtx_; // NOLINT
};
} // namespace utils::threading

#endif /* THREADING_H */
