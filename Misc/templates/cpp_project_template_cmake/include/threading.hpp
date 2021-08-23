#ifndef THREADING_H
#define THREADING_H

#include <iostream>
#include <sstream>
#include <mutex>

namespace utils::threading {
struct pcout : public std::stringstream {
    static inline std::mutex mtx_;
    ~pcout() override {
        std::lock_guard<std::mutex> _{mtx_};
        std::cout << rdbuf();
        std::cout.flush();
    }
};
} // namespace utils::threading

#endif /* THREADING_H */
