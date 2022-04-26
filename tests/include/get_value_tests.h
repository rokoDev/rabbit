#ifndef get_value_tests_h
#define get_value_tests_h

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
using SrcBitOffset = rabbit::SrcBitOffset;
using NumBits = rabbit::NumBits;
using Src = rabbit::Src;

using ThreeArgsT = std::tuple<std::string_view, SrcBitOffset, NumBits>;
using TwoArgsT = std::tuple<std::string_view, NumBits>;
using OneArgsT = std::tuple<std::string_view>;

template <typename DataT>
class GetValueBase : public TestWithParam<DataT>
{
   protected:
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
        helpers::to_uint8_buf(src_.data(), srcBitStr);

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
            return helpers::getValueExpected<T>(aSrcBitStr, aSrcOffset, aNBits);
        }
        else if constexpr (is_2_args_v)
        {
            const auto [aSrcBitStr, aNBits] = TestWithParam<DataT>::GetParam();
            return helpers::getValueExpected<T>(aSrcBitStr, SrcBitOffset{0},
                                                aNBits);
        }
        else
        {
            constexpr auto kNBits = NumBits{rabbit::utils::num_bits<T>()};
            const auto [aSrcBitStr] = TestWithParam<DataT>::GetParam();
            return helpers::getValueExpected<T>(aSrcBitStr, SrcBitOffset{0},
                                                kNBits);
        }
    }
    std::vector<uint8_t> src_;
};

using Args3U8 = GetValueBase<ThreeArgsT>;
using Args3U16 = GetValueBase<ThreeArgsT>;
using Args3U32 = GetValueBase<ThreeArgsT>;
using Args3U64 = GetValueBase<ThreeArgsT>;

using Args2U8 = GetValueBase<TwoArgsT>;
using Args2U16 = GetValueBase<TwoArgsT>;
using Args2U32 = GetValueBase<TwoArgsT>;
using Args2U64 = GetValueBase<TwoArgsT>;

using Args1U8 = GetValueBase<OneArgsT>;
using Args1U16 = GetValueBase<OneArgsT>;
using Args1U32 = GetValueBase<OneArgsT>;
using Args1U64 = GetValueBase<OneArgsT>;
}  // namespace test
}  // namespace rabbit

#endif /* get_value_tests_h */