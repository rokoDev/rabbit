#include "add_value_tests.h"

namespace
{
using namespace rabbit::test;

TEST_P(Args4UInt8, addValue)
{
    const auto kExpected = helpers::addValueExpected(GetParam());

    const auto [aDstBitStr, aValue, aOffset, aNBits] = GetParam();
    auto result = arrangeDst(aDstBitStr);
    rabbit::BinOps::addValue(result.data(), aOffset, aValue, aNBits);

    ASSERT_EQ(result, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args4UInt8);

TEST_P(Args4UInt16, addValue)
{
    const auto kExpected = helpers::addValueExpected(GetParam());

    const auto [aDstBitStr, aValue, aOffset, aNBits] = GetParam();
    auto result = arrangeDst(aDstBitStr);
    rabbit::BinOps::addValue(result.data(), aOffset, aValue, aNBits);

    ASSERT_EQ(result, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args4UInt16);

TEST_P(Args4UInt32, addValue)
{
    const auto kExpected = helpers::addValueExpected(GetParam());

    const auto [aDstBitStr, aValue, aOffset, aNBits] = GetParam();
    auto result = arrangeDst(aDstBitStr);
    rabbit::BinOps::addValue(result.data(), aOffset, aValue, aNBits);

    ASSERT_EQ(result, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args4UInt32);

TEST_P(Args4UInt64, addValue)
{
    const auto kExpected = helpers::addValueExpected(GetParam());

    const auto [aDstBitStr, aValue, aOffset, aNBits] = GetParam();
    auto result = arrangeDst(aDstBitStr);
    rabbit::BinOps::addValue(result.data(), aOffset, aValue, aNBits);

    ASSERT_EQ(result, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args4UInt64);

TEST_P(Args3UInt8, AddValue)
{
    const auto [aDstBitStr, aValue, aNBits] = GetParam();
    const auto kParam =
        QuadrupleData8T{aDstBitStr, aValue, DstBitOffset(0), aNBits};
    const auto kExpected = helpers::addValueExpected(kParam);

    auto result = arrangeDst(aDstBitStr);
    rabbit::BinOps::addValue(result.data(), aValue, aNBits);

    ASSERT_EQ(result, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args3UInt8);

TEST_P(Args3UInt16, AddValue)
{
    const auto [aDstBitStr, aValue, aNBits] = GetParam();
    const auto kParam =
        QuadrupleData16T{aDstBitStr, aValue, DstBitOffset(0), aNBits};
    const auto kExpected = helpers::addValueExpected(kParam);

    auto result = arrangeDst(aDstBitStr);
    rabbit::BinOps::addValue(result.data(), aValue, aNBits);

    ASSERT_EQ(result, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args3UInt16);

TEST_P(Args3UInt32, AddValue)
{
    const auto [aDstBitStr, aValue, aNBits] = GetParam();
    const auto kParam =
        QuadrupleData32T{aDstBitStr, aValue, DstBitOffset(0), aNBits};
    const auto kExpected = helpers::addValueExpected(kParam);

    auto result = arrangeDst(aDstBitStr);
    rabbit::BinOps::addValue(result.data(), aValue, aNBits);

    ASSERT_EQ(result, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args3UInt32);

TEST_P(Args3UInt64, AddValue)
{
    const auto [aDstBitStr, aValue, aNBits] = GetParam();
    const auto kParam =
        QuadrupleData64T{aDstBitStr, aValue, DstBitOffset(0), aNBits};
    const auto kExpected = helpers::addValueExpected(kParam);

    auto result = arrangeDst(aDstBitStr);
    rabbit::BinOps::addValue(result.data(), aValue, aNBits);

    ASSERT_EQ(result, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args3UInt64);

TEST_P(Args2UInt8, AddValue)
{
    const auto [aDstBitStr, aValue] = GetParam();
    using ValueT = std::decay_t<decltype(aValue)>;
    static_assert(rabbit::is_uint_v<ValueT>,
                  "ValueT must be unsigned integer type and not bool.");
    constexpr std::size_t kNBits = rabbit::utils::num_bits<ValueT>();
    const auto kParam =
        QuadrupleData8T{aDstBitStr, aValue, DstBitOffset(0), NumBits(kNBits)};
    const auto kExpected = helpers::addValueExpected(kParam);

    auto result = arrangeDst(aDstBitStr);
    rabbit::BinOps::addValue(result.data(), aValue);

    ASSERT_EQ(result, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args2UInt8);

TEST_P(Args2UInt16, AddValue)
{
    const auto [aDstBitStr, aValue] = GetParam();
    using ValueT = std::decay_t<decltype(aValue)>;
    static_assert(rabbit::is_uint_v<ValueT>,
                  "ValueT must be unsigned integer type and not bool.");
    constexpr std::size_t kNBits = rabbit::utils::num_bits<ValueT>();
    const auto kParam =
        QuadrupleData16T{aDstBitStr, aValue, DstBitOffset(0), NumBits(kNBits)};
    const auto kExpected = helpers::addValueExpected(kParam);

    auto result = arrangeDst(aDstBitStr);
    rabbit::BinOps::addValue(result.data(), aValue);

    ASSERT_EQ(result, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args2UInt16);

TEST_P(Args2UInt32, AddValue)
{
    const auto [aDstBitStr, aValue] = GetParam();
    using ValueT = std::decay_t<decltype(aValue)>;
    static_assert(rabbit::is_uint_v<ValueT>,
                  "ValueT must be unsigned integer type and not bool.");
    constexpr std::size_t kNBits = rabbit::utils::num_bits<ValueT>();
    const auto kParam =
        QuadrupleData32T{aDstBitStr, aValue, DstBitOffset(0), NumBits(kNBits)};
    const auto kExpected = helpers::addValueExpected(kParam);

    auto result = arrangeDst(aDstBitStr);
    rabbit::BinOps::addValue(result.data(), aValue);

    ASSERT_EQ(result, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args2UInt32);

TEST_P(Args2UInt64, AddValue)
{
    const auto [aDstBitStr, aValue] = GetParam();
    using ValueT = std::decay_t<decltype(aValue)>;
    static_assert(rabbit::is_uint_v<ValueT>,
                  "ValueT must be unsigned integer type and not bool.");
    constexpr std::size_t kNBits = rabbit::utils::num_bits<ValueT>();
    const auto kParam =
        QuadrupleData64T{aDstBitStr, aValue, DstBitOffset(0), NumBits(kNBits)};
    const auto kExpected = helpers::addValueExpected(kParam);

    auto result = arrangeDst(aDstBitStr);
    rabbit::BinOps::addValue(result.data(), aValue);

    ASSERT_EQ(result, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args2UInt64);
}  // namespace