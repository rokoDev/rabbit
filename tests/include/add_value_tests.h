#ifndef add_value_tests_h
#define add_value_tests_h

#include <gtest/gtest.h>

#include <string_view>
#include <vector>

#include "rabbit/bin_ops.h"
#include "test_helpers.h"

namespace rabbit
{
namespace test
{
using ::testing::TestWithParam;
using helpers = rabbit::test_helpers;
using DstBitOffset = rabbit::DstBitOffset;
using NumBits = rabbit::NumBits;
using Dst = rabbit::Dst;

template <typename DataT>
class AddValueBase : public TestWithParam<DataT>
{
   protected:
    static auto arrangeDst(std::string_view aDstBitStr)
    {
        std::vector<uint8_t> result(
            rabbit::details::bytesCount(aDstBitStr.size()));
        helpers::to_uint8_buf(result.data(), aDstBitStr);
        return result;
    }
};

using QuadrupleData64T =
    std::tuple<std::string_view, uint64_t, DstBitOffset, NumBits>;
using QuadrupleData32T =
    std::tuple<std::string_view, uint32_t, DstBitOffset, NumBits>;
using QuadrupleData16T =
    std::tuple<std::string_view, uint16_t, DstBitOffset, NumBits>;
using QuadrupleData8T =
    std::tuple<std::string_view, uint8_t, DstBitOffset, NumBits>;

using Args4UInt64 = AddValueBase<QuadrupleData64T>;
using Args4UInt32 = AddValueBase<QuadrupleData32T>;
using Args4UInt16 = AddValueBase<QuadrupleData16T>;
using Args4UInt8 = AddValueBase<QuadrupleData8T>;

using TripleData64T = std::tuple<std::string_view, uint64_t, NumBits>;
using TripleData32T = std::tuple<std::string_view, uint32_t, NumBits>;
using TripleData16T = std::tuple<std::string_view, uint16_t, NumBits>;
using TripleData8T = std::tuple<std::string_view, uint8_t, NumBits>;

using Args3UInt64 = AddValueBase<TripleData64T>;
using Args3UInt32 = AddValueBase<TripleData32T>;
using Args3UInt16 = AddValueBase<TripleData16T>;
using Args3UInt8 = AddValueBase<TripleData8T>;

using DoubleData64T = std::tuple<std::string_view, uint64_t>;
using DoubleData32T = std::tuple<std::string_view, uint32_t>;
using DoubleData16T = std::tuple<std::string_view, uint16_t>;
using DoubleData8T = std::tuple<std::string_view, uint8_t>;

using Args2UInt64 = AddValueBase<DoubleData64T>;
using Args2UInt32 = AddValueBase<DoubleData32T>;
using Args2UInt16 = AddValueBase<DoubleData16T>;
using Args2UInt8 = AddValueBase<DoubleData8T>;
}  // namespace test
}  // namespace rabbit

#endif /* add_value_tests_h */