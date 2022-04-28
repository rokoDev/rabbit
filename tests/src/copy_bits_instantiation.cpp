#include <fmt/core.h>
#include <gtest/gtest.h>

#include "copy_bits_tests.h"
#include "rabbit/user_literals.h"
#include "rabbit/utils.h"

namespace
{
using namespace rabbit::test;
using ::testing::Combine;
using ::testing::ValuesIn;

constexpr auto kDstBitOffsetsArray =
    rabbit::make_array_from_range<0_uf8, 7_uf8, 1_uf8, DstBitOffset>();
constexpr auto kSrcBitOffsetsArray =
    rabbit::make_array_from_range<0_uf8, 7_uf8, 1_uf8, SrcBitOffset>();
constexpr auto kBitOffsetsArray =
    rabbit::make_array_from_range<0_uf8, 7_uf8, 1_uf8, BitOffset>();
constexpr auto kNBitsArray =
    rabbit::make_array_from_range<0_uz, 70_uz, 1_uz, NumBits>();

const auto kTestDatas = TestDataFactory::presetData();

// copyBits tests instantiation
INSTANTIATE_TEST_SUITE_P(
    Rabbit, CopyBitsWith5Args,
    Combine(ValuesIn(kTestDatas), ValuesIn(kNBitsArray),
            ValuesIn(kDstBitOffsetsArray), ValuesIn(kSrcBitOffsetsArray)),
    [](const testing::TestParamInfo<CopyBitsWith5Args::ParamType> &aInfo) {
        const auto kTestData = std::get<0>(aInfo.param);
        const auto kNumBits = std::get<1>(aInfo.param);
        const auto kDstOffset = std::get<2>(aInfo.param);
        const auto kSrcOffset = std::get<3>(aInfo.param);
        return fmt::format("{}kNBits{}kDstOffset{}kSrcOffset{}",
                           kTestData->name(), kNumBits.get(), kDstOffset.get(),
                           kSrcOffset.get());
    });

INSTANTIATE_TEST_SUITE_P(
    Rabbit, CopyBitsWith4Args,
    Combine(ValuesIn(kTestDatas), ValuesIn(kNBitsArray),
            ValuesIn(kBitOffsetsArray)),
    [](const testing::TestParamInfo<CopyBitsWith4Args::ParamType> &aInfo) {
        const auto kTestData = std::get<0>(aInfo.param);
        const auto kNumBits = std::get<1>(aInfo.param);
        const auto kOffset = std::get<2>(aInfo.param);
        return fmt::format("{}kNBits{}kOffset{}", kTestData->name(),
                           kNumBits.get(), kOffset.get());
    });

INSTANTIATE_TEST_SUITE_P(
    Rabbit, CopyBitsWith3Args,
    Combine(ValuesIn(kTestDatas), ValuesIn(kNBitsArray)),
    [](const testing::TestParamInfo<CopyBitsWith3Args::ParamType> &aInfo) {
        const auto kTestData = std::get<0>(aInfo.param);
        const auto kNumBits = std::get<1>(aInfo.param);
        return fmt::format("{}kNBits{}", kTestData->name(), kNumBits.get());
    });
}  // namespace