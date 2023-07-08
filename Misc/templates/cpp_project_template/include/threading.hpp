#pragma once
#include <iostream>
#include <sstream>
#include <mutex>
#include <thread>
#include <algorithm>
#include <functional>

namespace utils::threading {
struct pcout : public std::stringstream {
    static inline std::mutex mtx_; // NOLINT
    ~pcout() override
    {
        std::lock_guard<std::mutex> _{mtx_};
        std::cout << rdbuf();
        std::cout.flush();
    }

    pcout() = default;
    pcout(pcout const&) = delete;
    pcout(pcout&&) noexcept = delete;
    pcout operator=(pcout const&) = delete;
    pcout operator=(pcout&&) noexcept = delete;
};

template <typename Iter>
void join_all(Iter first, Iter last)
{
    std::for_each(first, last, std::mem_fn(&std::thread::join));
}

template <typename Collection>
void join_all(Collection& collection)
{
    join_all(std::begin(collection), std::end(collection));
}
} // namespace utils::threading
