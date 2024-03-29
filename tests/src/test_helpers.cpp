#include "test_helpers.h"

#include <utils/utils.h>

#include <array>
#include <string_view>
#include <utility>

namespace test_utils
{
std::string random_bit_sequence(const std::size_t aNBits)
{
    std::random_device rd;
    std::mt19937 mt(rd());
    std::uniform_int_distribution<std::uint32_t> dist(0, 1);

    std::string result;
    result.reserve(aNBits);
    for (std::size_t i = 0; i < aNBits; ++i)
    {
        result += dist(mt) ? '1' : '0';
    }
    return result;
}
}  // namespace test_utils
