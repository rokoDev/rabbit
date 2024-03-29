#include <gtest/gtest.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include <sstream>

#include "add_value_tests.h"

namespace
{
using namespace std::string_view_literals;
using namespace ::test;
using ::testing::Combine;
using ::testing::Values;
using ::testing::ValuesIn;

inline constexpr auto kDstOffsets =
    utils::make_array_from_range<0_uf8, 7_uf8, 1_uf8, DstOffset>();

inline constexpr std::uint64_t kValue8 = 0b10110110;
inline constexpr auto kNBitsArray8 =
    utils::make_array_from_range<1_uz, 8_uz, 1_uz, NumBits>();

inline constexpr std::uint64_t kValue16 = 0b11110100'10000111;
inline constexpr auto kNBitsArray16 =
    utils::make_array_from_range<1_uz, 16_uz, 1_uz, NumBits>();

inline constexpr std::uint64_t kValue32 = 0b11100111'10110110'10111011'01111001;
inline constexpr auto kNBitsArray32 =
    utils::make_array_from_range<1_uz, 32_uz, 1_uz, NumBits>();

inline constexpr std::uint64_t kValue64 =
    0b11100111'10110110'10111011'01111001'00000010'01110000'11110100'10000111;
inline constexpr auto kNBitsArray64 =
    utils::make_array_from_range<1_uz, 64_uz, 1_uz, NumBits>();

// addValue tests instantiation
template <typename T>
std::string FourArgsDataName(
    const testing::TestParamInfo<typename T::ParamType> &aInfo)
{
    const auto kValue = std::get<1>(aInfo.param);
    const auto kDstOffset = std::get<2>(aInfo.param);
    const auto kNumBits = std::get<3>(aInfo.param);

    std::string initStr;
    initStr.reserve(64);
    std::stringstream ss(initStr);
    ss << "kValue" << std::to_string(kValue) << "kDstOffset"
       << std::to_string(kDstOffset.get()) << "kNBits"
       << std::to_string(kNumBits.get());
    return ss.str();
}

INSTANTIATE_TEST_SUITE_P(Rabbit, Args4UInt8,
                         Combine(Values(k72DstBits), Values(kValue8),
                                 ValuesIn(kDstOffsets), ValuesIn(kNBitsArray8)),
                         &FourArgsDataName<Args4UInt8>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args4UInt16,
                         Combine(Values(k72DstBits), Values(kValue16),
                                 ValuesIn(kDstOffsets),
                                 ValuesIn(kNBitsArray16)),
                         &FourArgsDataName<Args4UInt16>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args4UInt32,
                         Combine(Values(k72DstBits), Values(kValue32),
                                 ValuesIn(kDstOffsets),
                                 ValuesIn(kNBitsArray32)),
                         &FourArgsDataName<Args4UInt32>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args4UInt64,
                         Combine(Values(k72DstBits), Values(kValue64),
                                 ValuesIn(kDstOffsets),
                                 ValuesIn(kNBitsArray64)),
                         &FourArgsDataName<Args4UInt64>);

template <typename T>
std::string ThreeArgsDataName(
    const testing::TestParamInfo<typename T::ParamType> &aInfo)
{
    const auto kValue = std::get<1>(aInfo.param);
    const auto kNumBits = std::get<2>(aInfo.param);

    std::string initStr;
    initStr.reserve(64);
    std::stringstream ss(initStr);
    ss << "kValue" << std::to_string(kValue) << "kNBits"
       << std::to_string(kNumBits.get());
    return ss.str();
}

INSTANTIATE_TEST_SUITE_P(Rabbit, Args3UInt8,
                         Combine(Values(k72DstBits), Values(kValue8),
                                 ValuesIn(kNBitsArray8)),
                         &ThreeArgsDataName<Args3UInt8>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args3UInt16,
                         Combine(Values(k72DstBits), Values(kValue16),
                                 ValuesIn(kNBitsArray16)),
                         &ThreeArgsDataName<Args3UInt16>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args3UInt32,
                         Combine(Values(k72DstBits), Values(kValue32),
                                 ValuesIn(kNBitsArray32)),
                         &ThreeArgsDataName<Args3UInt32>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args3UInt64,
                         Combine(Values(k72DstBits), Values(kValue64),
                                 ValuesIn(kNBitsArray64)),
                         &ThreeArgsDataName<Args3UInt64>);

template <typename T>
std::string TwoArgsDataName(
    const testing::TestParamInfo<typename T::ParamType> &aInfo)
{
    const auto kValue = std::get<1>(aInfo.param);

    std::string initStr;
    initStr.reserve(64);
    std::stringstream ss(initStr);
    ss << "kValue" << std::to_string(kValue);
    return ss.str();
}

INSTANTIATE_TEST_SUITE_P(Rabbit, Args2UInt8,
                         Combine(Values(k72DstBits), Values(kValue8)),
                         &TwoArgsDataName<Args2UInt8>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args2UInt16,
                         Combine(Values(k72DstBits), Values(kValue16)),
                         &TwoArgsDataName<Args2UInt16>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args2UInt32,
                         Combine(Values(k72DstBits), Values(kValue32)),
                         &TwoArgsDataName<Args2UInt32>);

INSTANTIATE_TEST_SUITE_P(Rabbit, Args2UInt64,
                         Combine(Values(k72DstBits), Values(kValue64)),
                         &TwoArgsDataName<Args2UInt64>);
}  // namespace
