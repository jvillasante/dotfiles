#ifndef PCOUT_H
#define PCOUT_H

#include <iostream>
#include <mutex>
#include <sstream>

struct pcout : public std::stringstream {
    static inline std::mutex cout_mutex;

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

#endif /* PCOUT_H */
