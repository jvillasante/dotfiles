#if !defined __APPLE__

#include <ResourcePath.hpp>
#include <filesystem>

std::string resource_path() { return std::filesystem::canonical("/proc/self/exe").parent_path(); }

#endif
