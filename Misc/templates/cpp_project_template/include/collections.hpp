#pragma once
#include <algorithm>
#include <cassert>
#include <utility>
#include <vector>

namespace utils::collections
{
template <typename T>
void quick_remove_at(std::vector<T>& v,
                     typename std::vector<T>::size_type const idx)
{
    if (idx < v.size())
    {
        v[idx] = std::move(v.back());
        v.pop_back();
    }
}

template <typename T>
void quick_remove_at(std::vector<T>& v,
                     typename std::vector<T>::iterator const it)
{
    if (it != std::end(v))
    {
        *it = std::move(v.back());
        v.pop_back();
    }
}

template <typename C, typename T>
void insert_sorted(C& c, T const& value)
{
    assert(std::is_sorted(std::begin(c), std::end(c)) == true);
    auto const it = std::lower_bound(std::begin(c), std::end(c), value);
    c.insert(it, value);
}

// tells us the number of bytes that a collection owns
template <typename Collection>
std::size_t areaof(Collection const& x)
{
    using value_type = typename Collection::value_type;
    return (std::size(x) * sizeof(value_type)) + sizeof(Collection);
}

// how well do we use memory
template <typename Collection>
double memory_utilization(Collection const& x)
{
    using value_type = typename Collection::value_type;
    double useful(std::size(x) * sizeof(value_type));
    double total(areaof(x));
    return useful / total;
}
} // namespace utils::collections
