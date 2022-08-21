#include <gtest/gtest.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include <sstream>

#include "copy_bits_tests.h"

namespace
{
using namespace rabbit::test;
using ::testing::Combine;
using ::testing::ValuesIn;

constexpr auto kDstBitOffsetsArray =
    utils::make_array_from_range<0_uf8, 7_uf8, 1_uf8, DstBitOffset>();
constexpr auto kSrcBitOffsetsArray =
    utils::make_array_from_range<0_uf8, 7_uf8, 1_uf8, SrcBitOffset>();
constexpr auto kBitOffsetsArray =
    utils::make_array_from_range<0_uf8, 7_uf8, 1_uf8, BitOffset>();
constexpr auto kNBitsArray =
    utils::make_array_from_range<0_uz, 70_uz, 1_uz, NumBits>();

const auto kTestDatas = TestDataFactory::presetData();

// copyBits tests instantiation
INSTANTIATE_TEST_SUITE_P(
    Rabbit, CopyBitsWith5Args,
    Combine(ValuesIn(kTestDatas), ValuesIn(kNBitsArray),
            ValuesIn(kDstBitOffsetsArray), ValuesIn(kSrcBitOffsetsArray)),
    [](const testing::TestParamInfo<CopyBitsWith5Args::ParamType> &aInfo)
    {
        const auto kTestData = std::get<0>(aInfo.param);
        const auto kNumBits = std::get<1>(aInfo.param);
        const auto kDstOffset = std::get<2>(aInfo.param);
        const auto kSrcOffset = std::get<3>(aInfo.param);

        std::string initStr;
        initStr.reserve(128);
        std::stringstream ss(initStr);
        ss << kTestData->name() << "kNBits"sv << std::to_string(kNumBits.get())
           << "kDstOffset"sv << std::to_string(kDstOffset.get())
           << "kSrcOffset"sv << std::to_string(kSrcOffset.get());
        return ss.str();
    });

INSTANTIATE_TEST_SUITE_P(
    Rabbit, CopyBitsWith4Args,
    Combine(ValuesIn(kTestDatas), ValuesIn(kNBitsArray),
            ValuesIn(kBitOffsetsArray)),
    [](const testing::TestParamInfo<CopyBitsWith4Args::ParamType> &aInfo)
    {
        const auto kTestData = std::get<0>(aInfo.param);
        const auto kNumBits = std::get<1>(aInfo.param);
        const auto kOffset = std::get<2>(aInfo.param);

        std::string initStr;
        initStr.reserve(128);
        std::stringstream ss(initStr);
        ss << kTestData->name() << "kNBits"sv << std::to_string(kNumBits.get())
           << "kOffset"sv << std::to_string(kOffset.get());
        return ss.str();
    });

INSTANTIATE_TEST_SUITE_P(
    Rabbit, CopyBitsWith3Args,
    Combine(ValuesIn(kTestDatas), ValuesIn(kNBitsArray)),
    [](const testing::TestParamInfo<CopyBitsWith3Args::ParamType> &aInfo)
    {
        const auto kTestData = std::get<0>(aInfo.param);
        const auto kNumBits = std::get<1>(aInfo.param);

        std::string initStr;
        initStr.reserve(128);
        std::stringstream ss(initStr);
        ss << kTestData->name() << "kNBits"sv << std::to_string(kNumBits.get());
        return ss.str();
    });
}  // namespace
