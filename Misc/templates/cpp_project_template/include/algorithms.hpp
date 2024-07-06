#pragma once
#include <algorithm>
#include <iterator>
#include <functional>

namespace utils::algorithms
{
template <typename InputIt, typename OutputIt, typename T, typename F>
InputIt split(InputIt it, InputIt end_it, OutputIt out_it, T split_val, F bin_func)
{
    while (it != end_it)
    {
        auto const slice_end{std::find(it, end_it, split_val)};
        *out_it++ = bin_func(it, slice_end);

        if (slice_end == end_it) { return end_it; }
        it = std::next(slice_end);
    }

    return it;
}

template <typename It, typename F>
std::pair<It, It> gather(It first, It last, It gather_pos, F predicate)
{
    return std::make_pair(std::stable_partition(first, gather_pos, std::not_fn(predicate)),
                          std::stable_partition(gather_pos, last, predicate));
}

template <typename It>
void gather_sort(It first, It last, It gather_pos)
{
    using value_type = typename std::iterator_traits<It>::value_type;
    std::stable_sort(first, gather_pos, std::greater<value_type>{});
    std::stable_sort(gather_pos, last, std::less<value_type>{});
}
} // namespace utils::algorithms
