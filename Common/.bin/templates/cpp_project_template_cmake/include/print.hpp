#ifndef PRINT_H
#define PRINT_H

#include <iostream>
#include <string>
#include <string_view>
#include <sstream>
#include <utility>
#include <algorithm>
#include <vector>

#include <iterators.hpp>

// For printing map items
// NOLINTNEXTLINE
namespace std {
template <typename First, typename Second>
std::ostream& operator<<(std::ostream& os, std::pair<First, Second> const& p) {
    os << "(" << p.first << ", " << p.second << ")";
    return os;
}
} // namespace std

namespace utils::print {
inline std::ostream& line(char const c = '=', std::size_t const s = 80,
                          std::ostream& os = std::cout) {
    os << std::string(s, c) << '\n';
    return os;
}

inline std::ostream& line(std::string_view const header, char const c = '=',
                          std::size_t const s = 80, std::ostream& os = std::cout) {
    if (header.size() < s) {
        os << "===" << header << std::string(s - header.size() - 3, c) << '\n';
        return os;
    }

    os << header << '\n';
    return os;
}

inline std::ostream& new_line(std::string_view new_line_char = "\n", std::ostream& os = std::cout) {
    os << new_line_char;
    return os;
}

template <typename Iterator>
std::ostream& collection(Iterator begin, Iterator end, std::ostream& os = std::cout) {
    os << "[";
    if (begin != end) {
        std::copy(begin, end, utils::iterators::make_ostream_joiner(os, ", "));
    }
    os << "]";
    return os;
}

template <typename T>
std::ostream& vector(std::vector<T> const& vec, std::ostream& os = std::cout) {
    collection(std::cbegin(vec), std::cend(vec), os);
    os << "  Size:     " << vec.size() << '\n';
    os << "  Capacity: " << vec.capacity() << '\n';
    return os;
}
} // namespace utils::print

#endif /* PRINT_H */
