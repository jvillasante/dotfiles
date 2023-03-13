#ifndef THREADING_H
#define THREADING_H

#include <iostream>
#include <sstream>
#include <mutex>
#include <thread>
#include <algorithm>
#include <functional>

namespace utils::threading {
struct pcout : public std::stringstream {
    static inline std::mutex mtx_;
    ~pcout() override {
        std::lock_guard<std::mutex> _{mtx_};
        std::cout << rdbuf();
        std::cout.flush();
    }
};

template <typename Iter>
void wait_for_all(Iter first, Iter last) {
    std::for_each(first, last, std::mem_fn(&std::thread::join));
}
} // namespace utils::threading

#endif /* THREADING_H */
