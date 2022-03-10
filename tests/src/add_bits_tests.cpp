#include "add_bits_tests.h"

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

TEST_P(AddBitsWith5Args, TestFor)
{
    const auto [kData, kNBits, kDstOffset, kSrcOffset] = GetParam();
    const auto [kDstNBytes, kSrcNBytes] = bytesInDstSrc(GetParam());
    ASSERT_LE(kDstNBytes, kData->maxBytesInDst());
    ASSERT_LE(kSrcNBytes, kData->maxBytesInSrc());

    // arrange
    arrangeExpectedActual(GetParam());

    // act
    rabbit::BinOps::addBits(actual_.data(), kDstOffset, kData->srcBits().data(),
                            kSrcOffset, kNBits);

    // assert
    EXPECT_EQ(std::memcmp(actual_.data(), expected_.data(), expected_.size()),
              0);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(AddBitsWith5Args);

TEST_P(AddBitsWith4Args, TestFor)
{
    auto [kData, kNBits, kOffset] = GetParam();
    const auto kNBytes = bytesInDstSrc(GetParam());
    ASSERT_LE(kNBytes, kData->maxBytesInDst());
    ASSERT_LE(kNBytes, kData->maxBytesInSrc());

    // arrange
    const AddBits5TestDataT kParams{kData, kNBits, DstBitOffset(kOffset.get()),
                                    SrcBitOffset(kOffset.get())};
    arrangeExpectedActual(kParams);

    // act
    rabbit::BinOps::addBits(actual_.data(), kData->srcBits().data(), kOffset,
                            kNBits);

    // assert
    EXPECT_EQ(std::memcmp(actual_.data(), expected_.data(), expected_.size()),
              0);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(AddBitsWith4Args);

TEST_P(AddBitsWith3Args, TestFor)
{
    auto [kData, kNBits] = GetParam();
    const auto kNBytes = bytesInDstSrc(GetParam());
    ASSERT_LE(kNBytes, kData->maxBytesInDst());
    ASSERT_LE(kNBytes, kData->maxBytesInSrc());

    // arrange
    const AddBits5TestDataT kParams{kData, kNBits, DstBitOffset(0),
                                    SrcBitOffset(0)};
    arrangeExpectedActual(kParams);

    // act
    rabbit::BinOps::addBits(actual_.data(), kData->srcBits().data(), kNBits);

    // assert
    EXPECT_EQ(std::memcmp(actual_.data(), expected_.data(), expected_.size()),
              0);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(AddBitsWith3Args);
}  // namespace