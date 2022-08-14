#include "copy_bits_tests.h"

#include <algorithm>
#include <cstring>
#include <functional>
#include <memory>
#include <string>
#include <vector>

#include "rabbit/user_literals.h"

namespace
{
using namespace rabbit::test;

TEST_P(CopyBitsWith5Args, TestFor)
{
    const auto [kData, kNBits, kDstOffset, kSrcOffset] = GetParam();
    const auto [kDstNBytes, kSrcNBytes] = bytesInDstSrc(GetParam());
    ASSERT_LE(kDstNBytes, kData->maxBytesInDst());
    ASSERT_LE(kSrcNBytes, kData->maxBytesInSrc());

    // arrange
    arrangeExpectedActual(GetParam());

    // act
    rabbit::Core::copyBits(Dst(actual_.data()), kDstOffset,
                           Src(kData->srcBits().data()), kSrcOffset, kNBits);

    // assert
    EXPECT_EQ(std::memcmp(actual_.data(), expected_.data(), expected_.size()),
              0);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(CopyBitsWith5Args);

TEST_P(CopyBitsWith4Args, TestFor)
{
    auto [kData, kNBits, kOffset] = GetParam();
    const auto kNBytes = bytesInDstSrc(GetParam());
    ASSERT_LE(kNBytes, kData->maxBytesInDst());
    ASSERT_LE(kNBytes, kData->maxBytesInSrc());

    // arrange
    const CopyBits5TestDataT kParams{kData, kNBits, DstBitOffset(kOffset.get()),
                                     SrcBitOffset(kOffset.get())};
    arrangeExpectedActual(kParams);

    // act
    rabbit::Core::copyBits(Dst(actual_.data()), Src(kData->srcBits().data()),
                           kOffset, kNBits);

    // assert
    EXPECT_EQ(std::memcmp(actual_.data(), expected_.data(), expected_.size()),
              0);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(CopyBitsWith4Args);

TEST_P(CopyBitsWith3Args, TestFor)
{
    auto [kData, kNBits] = GetParam();
    const auto kNBytes = bytesInDstSrc(GetParam());
    ASSERT_LE(kNBytes, kData->maxBytesInDst());
    ASSERT_LE(kNBytes, kData->maxBytesInSrc());

    // arrange
    const CopyBits5TestDataT kParams{kData, kNBits, DstBitOffset(0),
                                     SrcBitOffset(0)};
    arrangeExpectedActual(kParams);

    // act
    rabbit::Core::copyBits(Dst(actual_.data()), Src(kData->srcBits().data()),
                           kNBits);

    // assert
    EXPECT_EQ(std::memcmp(actual_.data(), expected_.data(), expected_.size()),
              0);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(CopyBitsWith3Args);
}  // namespace
