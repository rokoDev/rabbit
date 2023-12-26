#include <gtest/gtest.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include <sstream>

#include "get_value_tests.h"

namespace
{
using namespace std::string_view_literals;
using namespace ::test;
using ::testing::Combine;
using ::testing::Values;
using ::testing::ValuesIn;

constexpr auto kSrcOffsetsArray =
    utils::make_array_from_range<0_uf8, 7_uf8, 1_uf8, SrcOffset>();

static inline constexpr auto k80SrcBits =
    "11001110110100010101100100001010011001011001101111100010101001111111010101100110"sv;

static inline constexpr auto kNBitsArray8 =
    utils::make_array_from_range<1_uz, 8_uz, 1_uz, NumBits>();

static inline constexpr auto kNBitsArray16 =
    utils::make_array_from_range<1_uz, 16_uz, 1_uz, NumBits>();

static inline constexpr auto kNBitsArray32 =
    utils::make_array_from_range<1_uz, 32_uz, 1_uz, NumBits>();

static inline constexpr auto kNBitsArray64 =
    utils::make_array_from_range<1_uz, 64_uz, 1_uz, NumBits>();

// getValue tests instantiation
template <typename T>
std::string GetValue3ArgsDataName(
    const testing::TestParamInfo<typename T::ParamType> &aInfo)
{
    const auto kSrcOffset = std::get<1>(aInfo.param);
    const auto kNumBits = std::get<2>(aInfo.param);

    std::string initStr;
    initStr.reserve(32);
    std::stringstream ss(initStr);
    ss << "kSrcOffset" << std::to_string(kSrcOffset.get()) << "kNBits"
       << std::to_string(kNumBits.get());
    return ss.str();
}

INSTANTIATE_TEST_SUITE_P(Rabbit, Args3U8,
                         Combine(Values(k80SrcBits), ValuesIn(kSrcOffsetsArray),
                                 ValuesIn(kNBitsArray8)),
                         &GetValue3ArgsDataName<Args3U8>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args3U16,
                         Combine(Values(k80SrcBits), ValuesIn(kSrcOffsetsArray),
                                 ValuesIn(kNBitsArray16)),
                         &GetValue3ArgsDataName<Args3U16>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args3U32,
                         Combine(Values(k80SrcBits), ValuesIn(kSrcOffsetsArray),
                                 ValuesIn(kNBitsArray32)),
                         &GetValue3ArgsDataName<Args3U32>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args3U64,
                         Combine(Values(k80SrcBits), ValuesIn(kSrcOffsetsArray),
                                 ValuesIn(kNBitsArray64)),
                         &GetValue3ArgsDataName<Args3U64>);

template <typename T>
std::string GetValue2ArgsDataName(
    const testing::TestParamInfo<typename T::ParamType> &aInfo)
{
    const auto kNumBits = std::get<1>(aInfo.param);

    std::string initStr;
    initStr.reserve(16);
    std::stringstream ss(initStr);
    ss << "kNBits" << std::to_string(kNumBits.get());
    return ss.str();
}

INSTANTIATE_TEST_SUITE_P(Rabbit, Args2U8,
                         Combine(Values(k80SrcBits), ValuesIn(kNBitsArray8)),
                         &GetValue2ArgsDataName<Args2U8>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args2U16,
                         Combine(Values(k80SrcBits), ValuesIn(kNBitsArray16)),
                         &GetValue2ArgsDataName<Args2U16>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args2U32,
                         Combine(Values(k80SrcBits), ValuesIn(kNBitsArray32)),
                         &GetValue2ArgsDataName<Args2U32>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args2U64,
                         Combine(Values(k80SrcBits), ValuesIn(kNBitsArray64)),
                         &GetValue2ArgsDataName<Args2U64>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args1U8, Combine(Values(k80SrcBits)));

INSTANTIATE_TEST_SUITE_P(Rabbit, Args1U16, Combine(Values(k80SrcBits)));

INSTANTIATE_TEST_SUITE_P(Rabbit, Args1U32, Combine(Values(k80SrcBits)));

INSTANTIATE_TEST_SUITE_P(Rabbit, Args1U64, Combine(Values(k80SrcBits)));
}  // namespace
