#ifndef UTILS_H
#define UTILS_H

#include <map>
#include <queue>
#include <numeric>
#include <iterator>
#include <ostream>
#include <type_traits>
#include <iostream>
#include <mutex>
#include <sstream>
#include <chrono>
#include <functional>
#include <string>
#include <string_view>
#include <vector>
#include <algorithm>
#include <utility>
#include <iomanip>
#include <memory>

// For printing map items
// NOLINTNEXTLINE
namespace std {
template <typename First, typename Second>
std::ostream& operator<<(std::ostream& os, const std::pair<First, Second>& p) {
    return os << "(" << p.first << ", " << p.second << ")";
}
} // namespace std

namespace utils {
template <typename... Ts>
constexpr void unused(Ts&&... ts) {
    ((void)ts, ...);
}

namespace functional {
namespace mutable_version {
template <typename F, typename R>
void mapf(F&& f, R& r) {
    std::transform(std::begin(r), std::end(r), std::begin(r), std::forward<F>(f));
}
} // namespace mutable_version

namespace variadic_function_template_version {
template <typename F, typename T1, typename T2>
auto foldl(F&& f, T1 arg1, T2 arg2) {
    return f(arg1, arg2);
}

template <typename F, typename T, typename... Ts>
auto foldl(F&& f, T&& head, Ts... rest) {
    return f(head, foldl(std::forward<F>(f), rest...));
}
} // namespace variadic_function_template_version

template <typename F, typename R>
R mapf(F&& f, R r) {
    std::transform(std::begin(r), std::end(r), std::begin(r), std::forward<F>(f));
    return r;
}

template <typename F, typename T, typename U>
std::map<T, U> mapf(F&& f, const std::map<T, U>& m) {
    std::map<T, U> r;
    for (auto&& kvp : m) {
        // r.insert(f(kvp));
        r.insert(f(std::forward<decltype(kvp)>(kvp)));
    }

    return r;
}

template <typename F, typename T>
std::queue<T> mapf(F&& f, std::queue<T> q) {
    std::queue<T> r;

    while (!q.empty()) {
        // r.push(f(q.front()));
        r.push(f(std::forward<T>(q.front())));
        q.pop();
    }

    return r;
}

template <typename F, typename R, typename T>
constexpr T foldl(F&& f, R&& r, T i) {
    return std::accumulate(std::begin(r), std::end(r), std::move(i), std::forward<F>(f));
}

template <typename F, typename R, typename T>
constexpr T foldr(F&& f, R&& r, T i) {
    // return std::accumulate(std::rbegin(r), std::rend(r), std::move(i), std::forward<F>(f));
    return std::accumulate(
        std::rbegin(r), std::rend(r), std::move(i), [f](auto&& arg1, auto&& arg2) {
            return f(std::forward<decltype(arg2)>(arg2), std::forward<decltype(arg1)>(arg1));
        });
}

template <typename F, typename T>
constexpr T foldl(F&& f, std::queue<T> q, T i) {
    while (!q.empty()) {
        // i = f(i, q.front());
        i = f(std::forward<T>(i), std::forward<T>(q.front()));
        q.pop();
    }

    return i;
}

template <typename F, typename G>
auto compose(F&& f, G&& g) {
    return [=](auto x) { return f(g(x)); };
}

template <typename F, typename... R>
auto compose(F&& f, R&&... r) {
    return [=](auto x) { return f(compose(r...)(x)); };
}
} // namespace functional

namespace iter {
template <class Delim, class CharT = char, class Traits = std::char_traits<CharT>>
class ostream_joiner {
public:
    using char_type = CharT;
    using traits_type = Traits;
    using ostream_type = std::basic_ostream<char_type, traits_type>;
    using iterator_category = std::output_iterator_tag;
    using value_type = void;
    using difference_type = void;
    using pointer = void;
    using reference = void;

    ostream_joiner(ostream_type& os, Delim&& d)
        : output_iter(std::addressof(os)), delim(std::move(d)), first(true) {}

    ostream_joiner(ostream_type& os, const Delim& d)
        : output_iter(std::addressof(os)), delim(d), first(true) {}

    template <typename Tp>
    ostream_joiner& operator=(const Tp& v) {
        if (!first) {
            *output_iter << delim;
        }

        first = false;
        *output_iter << v;
        return *this;
    }

    ostream_joiner& operator*() noexcept { return *this; }
    ostream_joiner& operator++() noexcept { return *this; }
    ostream_joiner& operator++(int) noexcept { return *this; }

private:
    ostream_type* output_iter;
    Delim delim;
    bool first;
};

template <class CharT, class Traits, class Delim>
ostream_joiner<typename std::decay<Delim>::type, CharT, Traits>
make_ostream_joiner(std::basic_ostream<CharT, Traits>& os, Delim&& d) {
    return ostream_joiner<typename std::decay<Delim>::type, CharT, Traits>(os,
                                                                           std::forward<Delim>(d));
}
} // namespace iter

namespace threading {
struct pcout : public std::stringstream {
    static inline std::mutex cout_mutex; // NOLINT

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
} // namespace threading

namespace bench {
//
// Perf timer implementation to measure functions
//
// Usage:
// auto t = perf_timer<std::chrono::microseconds>::duration(function_to_measure, arg1, arg2, arg3);
//
// auto t = perf_timer<>::duration([]() {
//   for (int i{0}; i < 10000; ++i) {
//     function_to_messure();
//   }
// });
//
// auto nanoseconds = std::chrono::duration<double, std::nano>(t).count();
// auto microseconds = std::chrono::duration<double, std::micro>(t).count();
// auto milliseconds = std::chrono::duration<double, std::milli>(t).count();
// auto seconds = std::chrono::duration<double>(t).count();
// auto minutes = std::chrono::duration<double, std::ratio<60>>(t).count();
// auto hours = std::chrono::duration<double, std::ratio<3600>>(t).count();
//
template <typename Time = std::chrono::microseconds,
          typename Clock = std::chrono::high_resolution_clock>
struct perf_timer {
    template <typename F, typename... Args>
    static Time duration(F&& f, Args&&... args) {
        auto start = Clock::now();

        std::invoke(std::forward<F>(f), std::forward<Args>(args)...);

        auto end = Clock::now();

        return std::chrono::duration_cast<Time>(end - start);
    }
};
} // namespace bench

namespace print {
inline void line(const char c = '=', const std::size_t s = 80) {
    std::cout << std::string(s, c) << '\n';
}

inline void line(std::string_view header, const char c = '=', const std::size_t s = 80) {
    if (header.size() < s) {
        std::cout << "===" << header << std::string(s - header.size() - 3, c) << '\n';
    } else {
        std::cout << header << '\n';
    }
}

inline void new_line() { std::cout << '\n'; }

template <typename Iterator>
void collection(std::string_view header, Iterator begin, Iterator end,
                std::ostream& out = std::cout) {
    line(header);
    out << "  [";
    if (begin != end) {
        std::copy(begin, end, iter::make_ostream_joiner(out, ", "));
    }
    out << "]\n";
}

template <typename Iterator>
void collection_inline(std::string_view header, Iterator begin, Iterator end,
                       std::ostream& out = std::cout) {
    out << header;
    out << " [";
    if (begin != end) {
        std::copy(begin, end, iter::make_ostream_joiner(out, ", "));
    }
    out << "]\n";
}

template <typename T>
void vector(std::string_view header, std::vector<T>& vec, std::ostream& out = std::cout) {
    collection(header, std::begin(vec), std::end(vec), out);

    out << "  Size:     " << vec.size() << '\n';
    out << "  Capacity: " << vec.capacity() << '\n';
}

template <typename T>
void vector_inline(std::string_view header, std::vector<T>& vec, std::ostream& out = std::cout) {
    collection_inline(header, std::begin(vec), std::end(vec), out);

    out << "  Size:     " << vec.size() << '\n';
    out << "  Capacity: " << vec.capacity() << '\n';
}
} // namespace print

namespace str {
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
} // namespace str

namespace smart_ptr {
/**
 * @brief Converts std::unique_ptr of base type to std::unique_ptr of derived type
 * by using static_cast internally
 * @details Ownership of the object is transfered to the returned std::unique_ptr.
 * It is somewhat analogous to std::static_ptr_cast
 */
template <typename Derived, typename Base, typename Deleter>
std::unique_ptr<Derived, Deleter> static_ptr_cast(std::unique_ptr<Base, Deleter> base) {
    auto deleter = base.get_deleter();
    auto derived_ptr = static_cast<Derived*>(base.release());
    return std::unique_ptr<Derived, Deleter>(derived_ptr, std::move(deleter));
}

/**
 * @details A separate static_ptr_cast() version for std::default_delete<T> is
 * required to provide expected results in use-cases like
 *
 * std::unique_ptr<Base> base(new Derived());
 * auto derived = static_ptr_cast<Derived>(std::move(base));
 *
 * It is desirable for `derived` to have type `std::unique_ptr<Derived>`,
 * but if general version of static_ptr_cast() is used, it will actually
 * be `std::unique_ptr<Derived, std::default_delete<Base>>` which is very
 * inconvenient
 */
template <typename Derived, typename Base>
std::unique_ptr<Derived> static_ptr_cast(std::unique_ptr<Base> base) noexcept {
    auto derived_ptr = static_cast<Derived*>(base.release());
    return std::unique_ptr<Derived>(derived_ptr);
}

/**
 * @brief Converts std::unique_ptr of base type to std::unique_ptr of derived type
 * by using dynamic_cast internally
 * @details Ownership of the object will be transfered to the returned std::unique_ptr
 * if and only if the dynamic_cast from Base to Derived is successful. Otherwise
 * the input std::unique_ptr will continue to be the owner of the object
 * It is somewhat analogous to std::dynamic_ptr_cast
 */
template <typename Derived, typename Base, typename Deleter>
std::unique_ptr<Derived, Deleter> dynamic_ptr_cast(std::unique_ptr<Base, Deleter>&& base) {
    if (auto derived = dynamic_cast<Derived*>(base.get())) {
        auto deleter = base.get_deleter();
        base.release();
        return std::unique_ptr<Derived, Deleter>(derived, std::move(deleter));
    }

    return nullptr;
}

/**
 * @details see description to @see static_ptr_cast() version that works only
 * with `std::default_delete<T>` for explanation why this special version of
 * dynamic_ptr_cast() is required
 */
template <typename Derived, typename Base>
std::unique_ptr<Derived> dynamic_ptr_cast(std::unique_ptr<Base>&& base) noexcept {
    if (auto derived = dynamic_cast<Derived*>(base.get())) {
        base.release();
        return std::unique_ptr<Derived>(derived);
    }

    return nullptr;
}
} // namespace smart_ptr
} // namespace utils

#endif /* UTILS_H */
