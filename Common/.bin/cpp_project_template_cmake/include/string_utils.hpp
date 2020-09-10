#ifndef STRING_UTILS_H
#define STRING_UTILS_H

#include <type_traits>
#include <string>
#include <sstream>

namespace string_utils {
template <class Val, class = typename std::enable_if<std::is_arithmetic<Val>::value>::type>
inline std::string to_string(Val const val) {
    return std::to_string(val);
}

template <class Val, class = typename std::enable_if<!std::is_arithmetic<Val>::value>::type>
inline std::string to_string(Val const& val) {
    return static_cast<std::ostringstream&>(std::ostringstream() << val).str();
}
} // namespace string_utils

#endif /* STRING_UTILS_H */
