#include "add_value_tests.h"

#include <endian/endian.h>
#include <utils/utils.h>

namespace
{
using namespace ::test;

TEST_P(Args4UInt8, addValue)
{
    const auto [aDstBitStr, aValue, aOffset, aNBits] = GetParam();
    const auto kExpectedBits = bit_helpers::addValueExpected<kBitCount>(
        aDstBitStr, aOffset, aValue, aNBits);
    const std::string_view kExpected{kExpectedBits.data(),
                                     kExpectedBits.size()};

    auto result = arrangeDst(aDstBitStr);
    core::add_value(Dst(result.data()), aOffset, aValue, aNBits);
    const auto kActualBits = bit_helpers::to_symbol_array<kBitCount>(result);
    const std::string_view kActual{kActualBits.data(), kActualBits.size()};

    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args4UInt8);

TEST_P(Args4UInt16, addValue)
{
    const auto [aDstBitStr, aValue, aOffset, aNBits] = GetParam();
    const auto kExpected = bit_helpers::addValueExpected<kBitCount>(
        aDstBitStr, aOffset, aValue, aNBits);

    auto result = arrangeDst(aDstBitStr);
    core::add_value(Dst(result.data()), aOffset, aValue, aNBits);
    const auto kActual = bit_helpers::to_symbol_array<kBitCount>(result);

    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args4UInt16);

TEST_P(Args4UInt32, addValue)
{
    const auto [aDstBitStr, aValue, aOffset, aNBits] = GetParam();
    const auto kExpected = bit_helpers::addValueExpected<kBitCount>(
        aDstBitStr, aOffset, aValue, aNBits);

    auto result = arrangeDst(aDstBitStr);
    core::add_value(Dst(result.data()), aOffset, aValue, aNBits);
    const auto kActual = bit_helpers::to_symbol_array<kBitCount>(result);

    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args4UInt32);

TEST_P(Args4UInt64, addValue)
{
    const auto [aDstBitStr, aValue, aOffset, aNBits] = GetParam();
    const auto kExpected = bit_helpers::addValueExpected<kBitCount>(
        aDstBitStr, aOffset, aValue, aNBits);

    auto result = arrangeDst(aDstBitStr);
    core::add_value(Dst(result.data()), aOffset, aValue, aNBits);
    const auto kActual = bit_helpers::to_symbol_array<kBitCount>(result);

    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args4UInt64);

TEST_P(Args3UInt8, AddValue)
{
    const auto [aDstBitStr, aValue, aNBits] = GetParam();
    const auto kExpected = bit_helpers::addValueExpected<kBitCount>(
        aDstBitStr, DstOffset(0), aValue, aNBits);

    auto result = arrangeDst(aDstBitStr);
    core::add_value(Dst(result.data()), aValue, aNBits);
    const auto kActual = bit_helpers::to_symbol_array<kBitCount>(result);

    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args3UInt8);

TEST_P(Args3UInt16, AddValue)
{
    const auto [aDstBitStr, aValue, aNBits] = GetParam();
    const auto kExpected = bit_helpers::addValueExpected<kBitCount>(
        aDstBitStr, DstOffset(0), aValue, aNBits);

    auto result = arrangeDst(aDstBitStr);
    core::add_value(Dst(result.data()), aValue, aNBits);
    const auto kActual = bit_helpers::to_symbol_array<kBitCount>(result);

    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args3UInt16);

TEST_P(Args3UInt32, AddValue)
{
    const auto [aDstBitStr, aValue, aNBits] = GetParam();
    const auto kExpected = bit_helpers::addValueExpected<kBitCount>(
        aDstBitStr, DstOffset(0), aValue, aNBits);

    auto result = arrangeDst(aDstBitStr);
    core::add_value(Dst(result.data()), aValue, aNBits);
    const auto kActual = bit_helpers::to_symbol_array<kBitCount>(result);

    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args3UInt32);

TEST_P(Args3UInt64, AddValue)
{
    const auto [aDstBitStr, aValue, aNBits] = GetParam();
    const auto kExpected = bit_helpers::addValueExpected<kBitCount>(
        aDstBitStr, DstOffset(0), aValue, aNBits);

    auto result = arrangeDst(aDstBitStr);
    core::add_value(Dst(result.data()), aValue, aNBits);
    const auto kActual = bit_helpers::to_symbol_array<kBitCount>(result);

    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args3UInt64);

TEST_P(Args2UInt8, AddValue)
{
    const auto [aDstBitStr, aValue] = GetParam();
    using ValueT = std::decay_t<decltype(aValue)>;
    static_assert(endian::is_uint_v<ValueT>,
                  "ValueT must be unsigned integer type and not bool.");
    constexpr auto kNBits = NumBits{utils::num_bits<ValueT>()};
    const auto kExpected = bit_helpers::addValueExpected<kBitCount>(
        aDstBitStr, DstOffset{0}, aValue, kNBits);

    auto result = arrangeDst(aDstBitStr);
    core::add_value(Dst(result.data()), aValue);
    const auto kActual = bit_helpers::to_symbol_array<kBitCount>(result);

    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args2UInt8);

TEST_P(Args2UInt16, AddValue)
{
    const auto [aDstBitStr, aValue] = GetParam();
    using ValueT = std::decay_t<decltype(aValue)>;
    static_assert(endian::is_uint_v<ValueT>,
                  "ValueT must be unsigned integer type and not bool.");
    constexpr auto kNBits = NumBits{utils::num_bits<ValueT>()};
    const auto kExpected = bit_helpers::addValueExpected<kBitCount>(
        aDstBitStr, DstOffset{0}, aValue, kNBits);

    auto result = arrangeDst(aDstBitStr);
    core::add_value(Dst(result.data()), aValue);
    const auto kActual = bit_helpers::to_symbol_array<kBitCount>(result);

    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args2UInt16);

TEST_P(Args2UInt32, AddValue)
{
    const auto [aDstBitStr, aValue] = GetParam();
    using ValueT = std::decay_t<decltype(aValue)>;
    static_assert(endian::is_uint_v<ValueT>,
                  "ValueT must be unsigned integer type and not bool.");
    constexpr auto kNBits = NumBits{utils::num_bits<ValueT>()};
    const auto kExpected = bit_helpers::addValueExpected<kBitCount>(
        aDstBitStr, DstOffset{0}, aValue, kNBits);

    auto result = arrangeDst(aDstBitStr);
    core::add_value(Dst(result.data()), aValue);
    const auto kActual = bit_helpers::to_symbol_array<kBitCount>(result);

    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args2UInt32);

TEST_P(Args2UInt64, AddValue)
{
    const auto [aDstBitStr, aValue] = GetParam();
    using ValueT = std::decay_t<decltype(aValue)>;
    static_assert(endian::is_uint_v<ValueT>,
                  "ValueT must be unsigned integer type and not bool.");
    constexpr auto kNBits = NumBits{utils::num_bits<ValueT>()};
    const auto kExpected = bit_helpers::addValueExpected<kBitCount>(
        aDstBitStr, DstOffset{0}, aValue, kNBits);

    auto result = arrangeDst(aDstBitStr);
    core::add_value(Dst(result.data()), aValue);
    const auto kActual = bit_helpers::to_symbol_array<kBitCount>(result);

    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args2UInt64);
}  // namespace
