//
// Copied from std::experimental implementation
//
#ifndef OSTREAM_JOINER_H
#define OSTREAM_JOINER_H

#include <iterator>
#include <ostream>
#include <type_traits>

namespace iterator_utils {
template <class _Delim, class _CharT = char, class _Traits = std::char_traits<_CharT>>
class ostream_joiner {
public:
    using char_type = _CharT;
    using traits_type = _Traits;
    using ostream_type = std::basic_ostream<char_type, traits_type>;
    using iterator_category = std::output_iterator_tag;
    using value_type = void;
    using difference_type = void;
    using pointer = void;
    using reference = void;

    ostream_joiner(ostream_type& __os, _Delim&& __d)
        : __output_iter(std::addressof(__os)), __delim(std::move(__d)), __first(true) {}

    ostream_joiner(ostream_type& __os, const _Delim& __d)
        : __output_iter(std::addressof(__os)), __delim(__d), __first(true) {}

    template <typename _Tp>
    ostream_joiner& operator=(const _Tp& __v) {
        if (!__first) {
            *__output_iter << __delim;
        }

        __first = false;
        *__output_iter << __v;
        return *this;
    }

    ostream_joiner& operator*() noexcept { return *this; }
    ostream_joiner& operator++() noexcept { return *this; }
    ostream_joiner& operator++(int) noexcept { return *this; }

private:
    ostream_type* __output_iter;
    _Delim __delim;
    bool __first;
};

template <class _CharT, class _Traits, class _Delim>
ostream_joiner<typename std::decay<_Delim>::type, _CharT, _Traits>
make_ostream_joiner(std::basic_ostream<_CharT, _Traits>& __os, _Delim&& __d) {
    return ostream_joiner<typename std::decay<_Delim>::type, _CharT, _Traits>(
        __os, std::forward<_Delim>(__d));
}
} // namespace iterator_utils

#endif /* OSTREAM_JOINER_H */
