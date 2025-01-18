#pragma once
#include <algorithm>
#include <iostream>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <iterators.hpp>

// For printing map items
namespace std
{
template <typename First, typename Second>
std::ostream& operator<<(std::ostream& os,
                         std::pair<First, Second> const& p) // NOLINT
{
    os << "(" << p.first << ':' << p.second << ")";
    return os;
}
} // namespace std

namespace utils::print
{
inline std::ostream& line(char const c = '=', std::size_t const s = 80,
                          std::ostream& os = std::cout)
{
    os << std::string(s, c) << '\n';
    return os;
}

inline std::ostream& line(std::string_view const header, char const c = '=',
                          std::size_t const s = 80,
                          std::ostream& os = std::cout)
{
    if (header.size() < s)
    {
        os << "===" << header << std::string(s - header.size() - 3, c) << '\n';
        return os;
    }

    os << header << '\n';
    return os;
}

inline std::ostream& new_line(std::string_view new_line_char = "\n",
                              std::ostream& os = std::cout)
{
    os << new_line_char;
    return os;
}

template <typename Iterator>
std::ostream& collection(Iterator begin, Iterator end,
                         char const start_char = '[', char const end_char = ']',
                         std::ostream& os = std::cout)
{
    os << start_char;
    std::copy(begin, end, utils::iterators::make_ostream_joiner(os, ", "));
    os << end_char << '\n';
    return os;
}

template <typename Collection>
std::ostream& collection(Collection const& c, char const start_char = '[',
                         char const end_char = ']',
                         std::ostream& os = std::cout)
{
    return collection(std::cbegin(c), std::cend(c), start_char, end_char, os);
}

template <typename T>
std::ostream& vector(std::vector<T> const& vec, std::ostream& os = std::cout)
{
    collection(std::cbegin(vec), std::cend(vec), '[', ']', os);
    os << "  Addr:     " << vec.data() << '\n';
    os << "  Size:     " << vec.size() << '\n';
    os << "  Capacity: " << vec.capacity() << '\n';
    return os;
}
} // namespace utils::print
