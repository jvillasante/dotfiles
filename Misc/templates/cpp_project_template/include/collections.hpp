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
} // namespace utils::collections
