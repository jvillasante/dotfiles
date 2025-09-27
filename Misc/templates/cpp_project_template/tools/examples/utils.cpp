#include <libutils/print.hpp>

#include <vector>

int main()
{
    std::vector<int> v{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    utils::print::vector(v);
    return 0;
}
