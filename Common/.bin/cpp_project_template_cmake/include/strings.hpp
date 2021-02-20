#ifndef STRINGS_H
#define STRINGS_H

#include <string>
#include <string_view>
#include <sstream>
#include <type_traits>
#include <algorithm>
#include <vector>
#include <iomanip>

namespace utils::strings {
template <typename CharT>
using tstring = std::basic_string<CharT, std::char_traits<CharT>, std::allocator<CharT>>;

template <typename CharT>
using tstringview = std::basic_string_view<CharT, std::char_traits<CharT>>;

template <typename CharT>
using tstringstream =
    std::basic_stringstream<CharT, std::char_traits<CharT>, std::allocator<CharT>>;
// ----------

template <typename Val, typename = typename std::enable_if<std::is_arithmetic<Val>::value>::type>
inline std::string to_string(Val const val) {
    return std::to_string(val);
}

template <typename Val, typename = typename std::enable_if<!std::is_arithmetic<Val>::value>::type>
inline std::string to_string(Val const& val) {
    return static_cast<std::ostringstream const&>(std::ostringstream{} << val).str();
}
// ----------

namespace mutable_version {
template <typename CharT>
inline void to_upper(tstring<CharT>& text) {
    std::transform(std::begin(text), std::end(text), std::begin(text), toupper);
}

template <typename CharT>
inline void to_lower(tstring<CharT>& text) {
    std::transform(std::begin(text), std::end(text), std::begin(text), tolower);
}

template <typename CharT>
inline void reverse(tstring<CharT>& text) {
    std::reverse(std::begin(text), std::end(text));
}

template <typename CharT>
inline void trim(tstring<CharT>& text) {
    auto first{text.find_first_not_of(' ')};
    auto last{text.find_last_not_of(' ')};
    text = text.substr(first, (last - first + 1));
}

template <typename CharT>
inline void trimleft(tstring<CharT>& text) {
    auto first{text.find_first_not_of(' ')};
    text = text.substr(first, text.size() - first);
}

template <typename CharT>
inline void trimright(tstring<CharT>& text) {
    auto last{text.find_last_not_of(' ')};
    text = text.substr(0, last + 1);
}
} // namespace mutable_version

template <typename CharT>
inline bool starts_with(tstringview<CharT> str, tstringview<CharT> prefix) {
    // return str.find(prefix) == 0;
    return str.substr(0, prefix.size()) == prefix;
}

template <typename CharT>
inline tstring<CharT> to_upper(tstring<CharT> text) {
    std::transform(std::begin(text), std::end(text), std::begin(text), toupper);
    return text;
}

template <typename CharT>
inline tstring<CharT> to_lower(tstring<CharT> text) {
    std::transform(std::begin(text), std::end(text), std::begin(text), tolower);
    return text;
}

template <typename CharT>
inline tstring<CharT> reverse(tstring<CharT> text) {
    std::reverse(std::begin(text), std::end(text));
    return text;
}

template <typename CharT>
inline tstring<CharT> trim(tstring<CharT> const& text) {
    auto first{text.find_first_not_of(' ')};
    auto last{text.find_last_not_of(' ')};
    return text.substr(first, (last - first + 1));
}

template <typename CharT>
inline tstring<CharT> trimleft(tstring<CharT> const& text) {
    auto first{text.find_first_not_of(' ')};
    return text.substr(first, text.size() - first);
}

template <typename CharT>
inline tstring<CharT> trimright(tstring<CharT> const& text) {
    auto last{text.find_last_not_of(' ')};
    return text.substr(0, last + 1);
}

template <typename CharT>
inline tstring<CharT> trim(tstring<CharT> const& text, tstring<CharT> const& chars) {
    auto first{text.find_first_not_of(chars)};
    auto last{text.find_last_not_of(chars)};
    return text.substr(first, (last - first + 1));
}

template <typename CharT>
inline tstring<CharT> trimleft(tstring<CharT> const& text, tstring<CharT> const& chars) {
    auto first{text.find_first_not_of(chars)};
    return text.substr(first, text.size() - first);
}

template <typename CharT>
inline tstring<CharT> trimright(tstring<CharT> const& text, tstring<CharT> const& chars) {
    auto last{text.find_last_not_of(chars)};
    return text.substr(0, last + 1);
}

template <typename CharT>
inline tstring<CharT> remove(tstring<CharT> text, CharT const ch) {
    auto start =
        std::remove_if(std::begin(text), std::end(text), [=](CharT const c) { return c == ch; });
    text.erase(start, std::end(text));
    return text;
}

template <typename CharT, typename Iter>
inline tstring<CharT> join(Iter begin, Iter end, CharT const* const separator) {
    tstringstream<CharT> oss;
    std::copy(begin, std::prev(end), std::ostream_iterator<tstring<CharT>>(oss, separator));
    oss << *std::prev(end);
    return oss.str();
}

template <typename CharT, typename C>
inline tstring<CharT> join(C const& c, CharT const* const separator) {
    if (c.empty()) {
        return {};
    }

    return join(std::cbegin(c), std::cend(c), separator);
}

template <typename CharT>
inline std::vector<tstring<CharT>> split(tstring<CharT> text, CharT const delimiter) {
    auto sstr = tstringstream<CharT>{std::move(text)};
    auto tokens = std::vector<tstring<CharT>>{};
    auto token = tstring<CharT>{};
    while (std::getline(sstr, token, delimiter)) {
        if (!token.empty()) {
            tokens.push_back(token);
        }
    }

    return tokens;
}

template <typename CharT>
inline std::vector<tstring<CharT>> split(tstring<CharT> const& text,
                                         tstring<CharT> const& delimiters) {
    auto tokens = std::vector<tstring<CharT>>{};

    std::size_t pos = 0;
    std::size_t prev_pos = 0;
    while ((pos = text.find_first_of(delimiters, prev_pos)) != std::string::npos) {
        if (pos > prev_pos) {
            tokens.emplace_back(text.substr(prev_pos, pos - prev_pos));
        }

        prev_pos = pos + 1;
    }

    if (prev_pos < text.size()) {
        tokens.emplace_back(text.substr(prev_pos, std::string::npos));
    }

    return tokens;
}

template <typename CharT, typename InputIter>
inline tstring<CharT> to_hex(InputIter first, InputIter last, bool use_uppercase = true,
                             bool insert_spaces = false) {
    tstringstream<CharT> ss;
    ss << std::hex << std::setfill('0');
    if (use_uppercase) {
        ss << std::uppercase;
    }

    while (first != last) {
        ss << std::setw(2) << static_cast<int>(*first++);
        if (insert_spaces && first != last) {
            ss << ' ';
        }
    }
    return ss.str();
}

template <typename CharT, typename C>
inline tstring<CharT> to_hex(C const& c, bool use_uppercase = true, bool insert_spaces = false) {
    if (c.empty()) {
        return {};
    }

    return to_hex(std::cbegin(c), std::cend(c), use_uppercase, insert_spaces);
}
} // namespace utils::strings

#endif /* STRINGS_H */
