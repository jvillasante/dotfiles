#ifndef PRINT_H
#define PRINT_H

#include <iostream>
#include <string>
#include <string_view>
#include <vector>
#include <algorithm>
#include <type_traits>
#include <utility>
#include <cassert>

#include "ostream_joiner.hpp"

// For printing map items
namespace std {
template <typename First, typename Second>
std::ostream& operator<<(std::ostream& os, const std::pair<First, Second>& p) {
    return os << "(" << p.first << ", " << p.second << ")";
}
} // namespace std

namespace print {
void line(const char c = '=', const std::size_t s = 80) {
    std::cout << std::string(s, c) << '\n';
}

void line(std::string_view header, const char c = '=',
          const std::size_t s = 80) {
    assert(header.size() < s);

    std::cout << "===" << header << std::string(s - header.size() - 3, c)
              << '\n';
}

void new_line() { std::cout << '\n'; }

template <typename Iterator>
void collection(std::string_view header, Iterator begin, Iterator end,
                std::ostream& out = std::cout) {
    line(header);
    out << "  [";
    if (begin != end) {
        std::copy(begin, end, iterator_utils::make_ostream_joiner(out, ", "));
    }
    out << "]\n";
}

template <typename Iterator>
void collection_inline(std::string_view header, Iterator begin, Iterator end,
                       std::ostream& out = std::cout) {
    out << header;
    out << " [";
    if (begin != end) {
        std::copy(begin, end, iterator_utils::make_ostream_joiner(out, ", "));
    }
    out << "]\n";
}

template <typename T>
void vector(std::string_view header, std::vector<T>& vec,
            std::ostream& out = std::cout) {
    collection(header, std::begin(vec), std::end(vec), out);

    out << "  Size:     " << vec.size() << '\n';
    out << "  Capacity: " << vec.capacity() << '\n';
}

template <typename T>
void vector_inline(std::string_view header, std::vector<T>& vec,
                   std::ostream& out = std::cout) {
    collection_inline(header, std::begin(vec), std::end(vec), out);

    out << "  Size:     " << vec.size() << '\n';
    out << "  Capacity: " << vec.capacity() << '\n';
}

} // namespace print

#endif /* PRINT_H */