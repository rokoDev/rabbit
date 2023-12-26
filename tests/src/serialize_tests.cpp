#include <gtest/gtest.h>
#include <rabbit/serialize.h>
#include <user_literals/user_literals.h>

#include "serialization_tests.h"

namespace rabbit
{
template <typename T>
using enable_if_uint16_t =
    std::enable_if_t<std::is_same_v<std::decay_t<T>, std::uint16_t>, T>;

template <typename T>
result<void> serialize(writer &aWriter, T &&aValue,
                       tag_t<enable_if_uint16_t<T>>) noexcept
{
    BOOST_LEAF_CHECK(aWriter.addValue(aValue, NumBits(10)));
    return {};
}
}  // namespace rabbit

TEST(SerializeTests, UInt16)
{
    std::byte rawData[16]{};
    auto writer_result = rabbit::make_bin_writer(rawData);
    ASSERT_TRUE(writer_result.has_value());
    std::uint16_t valueToSave{1012_u16};
    auto r = rabbit::serialize(*writer_result, valueToSave);
    ASSERT_TRUE(r);
    ASSERT_EQ(rawData[0], 0b11111101_b);
    ASSERT_EQ(rawData[1], 0b00000000_b);
    ASSERT_EQ((*writer_result).pos(), bit_pos(10));
}

TEST(SerializeTests, Empty)
{
    std::byte rawData[16]{};
    auto writer_result = rabbit::make_bin_writer(rawData);
    ASSERT_TRUE(writer_result.has_value());
    Empty valueToSave;
    auto r = rabbit::serialize(*writer_result, valueToSave);
    ASSERT_TRUE(r);
    ASSERT_EQ(rawData[0], 0b00000000_b);
    ASSERT_EQ(rawData[1], 0b00000000_b);
    ASSERT_EQ((*writer_result).pos(), bit_pos(0));
}

TEST(SerializeTests, NonEmpty)
{
    std::byte rawData[16]{};
    auto writer_result = rabbit::make_bin_writer(rawData);
    ASSERT_TRUE(writer_result.has_value());
    NotEmpty valueToSave{1_u16, 3_u16};
    auto r = rabbit::serialize(*writer_result, valueToSave);
    ASSERT_TRUE(r);
    ASSERT_EQ(rawData[0], 0b00000000_b);
    ASSERT_EQ(rawData[1], 0b01000000_b);
    ASSERT_EQ(rawData[2], 0b00110000_b);
    ASSERT_EQ(rawData[3], 0b00000000_b);
    ASSERT_EQ(rawData[4], 0b00000000_b);
    ASSERT_EQ((*writer_result).pos(), bit_pos(20));
}

TEST(SerializeTests, Nested)
{
    std::byte rawData[16]{};
    auto writer_result = rabbit::make_bin_writer(rawData);
    ASSERT_TRUE(writer_result.has_value());
    Nested valueToSave{0b0000001001111001_u16,
                       {0b0000000000100000_u16, 0b0000001101101100_u16},
                       0b0000001000110001_u16};
    auto r = rabbit::serialize(*writer_result, valueToSave);
    ASSERT_TRUE(r);
    ASSERT_EQ(rawData[0], 0b10011110_b);
    ASSERT_EQ(rawData[1], 0b01000010_b);
    ASSERT_EQ(rawData[2], 0b00001101_b);
    ASSERT_EQ(rawData[3], 0b10110010_b);
    ASSERT_EQ(rawData[4], 0b00110001_b);
    ASSERT_EQ(rawData[5], 0b00000000_b);
    ASSERT_EQ((*writer_result).pos(), bit_pos(40));
}
