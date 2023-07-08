#pragma once
#include <iterator>

namespace utils::iterators {
template <typename Delim, typename CharT = char, typename Traits = std::char_traits<CharT>>
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

    ostream_joiner(ostream_type& os, Delim&& d) : output_iter(std::addressof(os)), delim(std::move(d)), first(true) {}

    ostream_joiner(ostream_type& os, const Delim& d) : output_iter(std::addressof(os)), delim(d), first(true) {}

    template <typename Tp>
    ostream_joiner& operator=(const Tp& v)
    {
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
inline ostream_joiner<typename std::decay<Delim>::type, CharT, Traits> make_ostream_joiner(std::basic_ostream<CharT, Traits>& os,
                                                                                           Delim&& d)
{
    return ostream_joiner<typename std::decay<Delim>::type, CharT, Traits>(os, std::forward<Delim>(d));
}
} // namespace utils::iterators
