#ifndef get_value_tests_h
#define get_value_tests_h

#include <gtest/gtest.h>
#include <utils/utils.h>

#include <string_view>
#include <vector>

#include "rabbit/bin_ops.h"
#include "test_helpers.h"

namespace test
{
#ifdef CORE_V1
using core_t = ::rabbit::v1::core;
#endif

#ifdef CORE_V2
using core_t = ::rabbit::v2::core;
#endif

using ::testing::TestWithParam;
namespace helpers = ::test_utils;
using bit_helpers = helpers::bits<core_t>;
using SrcOffset = ::rabbit::SrcOffset;
using NumBits = ::rabbit::NumBits;
using Src = ::rabbit::Src;
using Dst = ::rabbit::Dst;

using ThreeArgsT = std::tuple<std::string_view, SrcOffset, NumBits>;
using TwoArgsT = std::tuple<std::string_view, NumBits>;
using OneArgsT = std::tuple<std::string_view>;

template <typename DataT, typename UInt>
class GetValueBase : public TestWithParam<DataT>
{
   protected:
    using core = core_t;
    using U = UInt;
    auto getArgs()
    {
        constexpr bool is_3_args_v = std::is_same_v<DataT, ThreeArgsT>;
        constexpr bool is_2_args_v = std::is_same_v<DataT, TwoArgsT>;
        constexpr bool is_1_args_v = std::is_same_v<DataT, OneArgsT>;
        static_assert(is_3_args_v || is_2_args_v || is_1_args_v,
                      "Invalid DataT");

        std::string_view srcBitStr =
            std::get<0>(TestWithParam<DataT>::GetParam());
        src_.reserve(srcBitStr.size());
        bit_helpers::to_byte_buf(Dst{src_.data()}, srcBitStr);

        if constexpr (is_3_args_v)
        {
            auto kSrcOffset = std::get<1>(TestWithParam<DataT>::GetParam());
            auto kNBits = std::get<2>(TestWithParam<DataT>::GetParam());
            return std::make_tuple(src_.data(), kSrcOffset, kNBits);
        }
        else if constexpr (is_2_args_v)
        {
            auto kNBits = std::get<1>(TestWithParam<DataT>::GetParam());
            return std::make_tuple(src_.data(), kNBits);
        }
        else
        {
            return std::make_tuple(src_.data());
        }
    }

    template <typename T>
    static auto expectedValue()
    {
        constexpr bool is_3_args_v = std::is_same_v<DataT, ThreeArgsT>;
        constexpr bool is_2_args_v = std::is_same_v<DataT, TwoArgsT>;
        constexpr bool is_1_args_v = std::is_same_v<DataT, OneArgsT>;
        static_assert(is_3_args_v || is_2_args_v || is_1_args_v,
                      "Invalid DataT");
        if constexpr (is_3_args_v)
        {
            const auto [aSrcBitStr, aSrcOffset, aNBits] =
                TestWithParam<DataT>::GetParam();
            return bit_helpers::getValueExpected<T>(aSrcBitStr, aSrcOffset,
                                                    aNBits);
        }
        else if constexpr (is_2_args_v)
        {
            const auto [aSrcBitStr, aNBits] = TestWithParam<DataT>::GetParam();
            return bit_helpers::getValueExpected<T>(aSrcBitStr, SrcOffset{0},
                                                    aNBits);
        }
        else
        {
            constexpr auto kNBits = NumBits{utils::num_bits<T>()};
            const auto [aSrcBitStr] = TestWithParam<DataT>::GetParam();
            return bit_helpers::getValueExpected<T>(aSrcBitStr, SrcOffset{0},
                                                    kNBits);
        }
    }
    std::vector<std::byte> src_;
};

using Args3U8 = GetValueBase<ThreeArgsT, std::uint8_t>;
using Args3U16 = GetValueBase<ThreeArgsT, std::uint16_t>;
using Args3U32 = GetValueBase<ThreeArgsT, std::uint32_t>;
using Args3U64 = GetValueBase<ThreeArgsT, std::uint64_t>;

using Args2U8 = GetValueBase<TwoArgsT, std::uint8_t>;
using Args2U16 = GetValueBase<TwoArgsT, std::uint16_t>;
using Args2U32 = GetValueBase<TwoArgsT, std::uint32_t>;
using Args2U64 = GetValueBase<TwoArgsT, std::uint64_t>;

using Args1U8 = GetValueBase<OneArgsT, std::uint8_t>;
using Args1U16 = GetValueBase<OneArgsT, std::uint16_t>;
using Args1U32 = GetValueBase<OneArgsT, std::uint32_t>;
using Args1U64 = GetValueBase<OneArgsT, std::uint64_t>;
}  // namespace test

#endif /* get_value_tests_h */
