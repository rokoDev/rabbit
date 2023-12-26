#ifndef add_value_tests_h
#define add_value_tests_h

#include <gtest/gtest.h>

#include <string_view>
#include <vector>

#include "rabbit/bin_ops.h"
#include "test_helpers.h"

namespace test
{
#ifdef CORE_V1
using core_t = ::rabbit::v1::Core;
#endif

#ifdef CORE_V2
using core_t = ::rabbit::v2::Core;
#endif
using ::testing::TestWithParam;
using namespace std::string_view_literals;
namespace helpers = ::test_utils;
using bit_helpers = helpers::bits<core_t>;
using DstOffset = ::rabbit::DstOffset;
using NumBits = ::rabbit::NumBits;
using Dst = ::rabbit::Dst;

inline constexpr auto k72DstBits =
    "101010100100101010101101111001000110100000000111011111110001011011110101"sv;

template <typename DataT>
class AddValueBase : public TestWithParam<DataT>
{
   protected:
    using core = core_t;
    static constexpr auto kBitCount = k72DstBits.size();
    static auto arrangeDst(std::string_view aDstBitStr)
    {
        using ::rabbit::details::bytes_count;
        std::array<std::byte, bytes_count(NumBits{kBitCount})> result{};
        bit_helpers::to_byte_buf(Dst{result.data()}, aDstBitStr);
        return result;
    }
};

using QuadrupleData64T =
    std::tuple<std::string_view, std::uint64_t, DstOffset, NumBits>;
using QuadrupleData32T =
    std::tuple<std::string_view, std::uint32_t, DstOffset, NumBits>;
using QuadrupleData16T =
    std::tuple<std::string_view, std::uint16_t, DstOffset, NumBits>;
using QuadrupleData8T =
    std::tuple<std::string_view, std::uint8_t, DstOffset, NumBits>;

using Args4UInt64 = AddValueBase<QuadrupleData64T>;
using Args4UInt32 = AddValueBase<QuadrupleData32T>;
using Args4UInt16 = AddValueBase<QuadrupleData16T>;
using Args4UInt8 = AddValueBase<QuadrupleData8T>;

using TripleData64T = std::tuple<std::string_view, std::uint64_t, NumBits>;
using TripleData32T = std::tuple<std::string_view, std::uint32_t, NumBits>;
using TripleData16T = std::tuple<std::string_view, std::uint16_t, NumBits>;
using TripleData8T = std::tuple<std::string_view, std::uint8_t, NumBits>;

using Args3UInt64 = AddValueBase<TripleData64T>;
using Args3UInt32 = AddValueBase<TripleData32T>;
using Args3UInt16 = AddValueBase<TripleData16T>;
using Args3UInt8 = AddValueBase<TripleData8T>;

using DoubleData64T = std::tuple<std::string_view, std::uint64_t>;
using DoubleData32T = std::tuple<std::string_view, std::uint32_t>;
using DoubleData16T = std::tuple<std::string_view, std::uint16_t>;
using DoubleData8T = std::tuple<std::string_view, std::uint8_t>;

using Args2UInt64 = AddValueBase<DoubleData64T>;
using Args2UInt32 = AddValueBase<DoubleData32T>;
using Args2UInt16 = AddValueBase<DoubleData16T>;
using Args2UInt8 = AddValueBase<DoubleData8T>;
}  // namespace test

#endif /* add_value_tests_h */
