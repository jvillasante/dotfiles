#pragma once
#include <algorithm>
#include <functional>
#include <iostream>
#include <memory>
#include <mutex>
#include <sstream>
#include <thread>

namespace utils::threading
{
struct pcout : public std::stringstream
{
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

/**
 * The anti-lock unlocks a `mutex` at construction and locks it at destruction.
 */
template <typename Guard>
struct anti_lock
{
    using mutex_type = typename Guard::mutex_type;

    explicit anti_lock(Guard& guard) : mutex_(guard.mutex())
    {
        if (mutex_) { mutex_->unlock(); }
    }

private:
    struct anti_lock_deleter
    {
        void operator()(mutex_type* mutex) { mutex->lock(); }
    };

    std::unique_ptr<mutex_type, anti_lock_deleter> mutex_;
};
} // namespace utils::threading
