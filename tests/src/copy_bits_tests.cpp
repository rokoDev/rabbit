#include "copy_bits_tests.h"

#include <user_literals/user_literals.h>

#include <algorithm>
#include <cstring>
#include <functional>
#include <memory>
#include <string>
#include <vector>

namespace
{
using namespace ::test;

TEST_P(CopyBits5Args, TestFor)
{
    auto [kDstData, kSrcData, kNBits, kDstOffset, kSrcOffset] = GetParam();

    // arrange
    auto expected_dst_bits =
        std::vector<char>{kDstData.str.cbegin(), kDstData.str.cend()};
    test_utils::copy_bits_expected_str(expected_dst_bits.data(),
                                       expected_dst_bits.size(), kDstOffset,
                                       kSrcData.str, kSrcOffset, kNBits);
    auto expected =
        std::string_view(expected_dst_bits.data(), expected_dst_bits.size());
    auto actual_dst_buf = to_buf(kDstData);

    // act
    core::copy(Dst(actual_dst_buf.data()), kDstOffset, kSrcData.bytes,
               kSrcOffset, kNBits);
    auto actual_dst_bits = to_bits_str(actual_dst_buf);
    auto actual =
        std::string_view(actual_dst_bits.data(), actual_dst_bits.size());

    // assert
    ASSERT_EQ(actual, expected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(CopyBitsWith5Args);

TEST_P(CopyBits4Args, TestFor)
{
    auto [kDstData, kSrcData, kNBits, kOffset] = GetParam();

    // arrange
    auto expected_dst_bits =
        std::vector<char>{kDstData.str.cbegin(), kDstData.str.cend()};
    test_utils::copy_bits_expected_str(
        expected_dst_bits.data(), expected_dst_bits.size(), DstOffset{kOffset},
        kSrcData.str, SrcOffset{kOffset}, kNBits);
    auto expected =
        std::string_view(expected_dst_bits.data(), expected_dst_bits.size());
    auto actual_dst_buf = to_buf(kDstData);

    // act
    core::copy(Dst(actual_dst_buf.data()), kSrcData.bytes, kOffset, kNBits);
    auto actual_dst_bits = to_bits_str(actual_dst_buf);
    auto actual =
        std::string_view(actual_dst_bits.data(), actual_dst_bits.size());

    // assert
    ASSERT_EQ(actual, expected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(CopyBitsWith4Args);

TEST_P(CopyBits3Args, TestFor)
{
    auto [kDstData, kSrcData, kNBits] = GetParam();

    // arrange
    auto expected_dst_bits =
        std::vector<char>{kDstData.str.cbegin(), kDstData.str.cend()};
    test_utils::copy_bits_expected_str(expected_dst_bits.data(),
                                       expected_dst_bits.size(), DstOffset{0},
                                       kSrcData.str, SrcOffset{0}, kNBits);
    auto expected =
        std::string_view(expected_dst_bits.data(), expected_dst_bits.size());
    auto actual_dst_buf = to_buf(kDstData);

    // act
    core::copy(Dst(actual_dst_buf.data()), kSrcData.bytes, kNBits);
    auto actual_dst_bits = to_bits_str(actual_dst_buf);
    auto actual =
        std::string_view(actual_dst_bits.data(), actual_dst_bits.size());

    // assert
    ASSERT_EQ(actual, expected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(CopyBitsWith3Args);
}  // namespace
