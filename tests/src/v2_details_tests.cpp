#include <gtest/gtest.h>
#include <user_literals/user_literals.h>

#include <array>
#include <cstddef>
#include <cstdint>

#include "rabbit/bin_ops.h"

namespace
{
namespace bits = ::rabbit::v2::details;
using Src = ::rabbit::Src;
class ByteArrays : public ::testing::Test
{
   protected:
    static constexpr std::size_t BufSize = 8;
    using ArrT = std::array<std::byte, BufSize>;
    const ArrT src_{0b10101111_b, 0b00010111_b, 0b00001111_b, 0b00110011_b,
                    0b10101010_b, 0b01111110_b, 0b10000001_b, 0b10011001_b};
    ArrT dst_{0b10100101_b, 0b11111111_b, 0b01010101_b, 0b10101010_b,
              0b10100101_b, 0b11111111_b, 0b01010101_b, 0b10101010_b};
};

using ToUInt8AsLE = ByteArrays;
using ToUInt16AsLE = ByteArrays;
using ToUInt32AsLE = ByteArrays;
using ToUInt64AsLE = ByteArrays;

TEST_F(ToUInt8AsLE, OneByte)
{
    auto value = bits::to_ule<std::uint8_t, 1>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint8_t>);
    ASSERT_EQ(value, 0b10101111_u8);
}

TEST_F(ToUInt16AsLE, OneByte)
{
    auto value = bits::to_ule<std::uint16_t, 1>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint16_t>);
    ASSERT_EQ(value, 0b10101111_u16);
}

TEST_F(ToUInt16AsLE, TwoBytes)
{
    auto value = bits::to_ule<std::uint16_t, 2>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint16_t>);
    ASSERT_EQ(value, 0b00010111'10101111_u16);
}

TEST_F(ToUInt32AsLE, OneByte)
{
    auto value = bits::to_ule<std::uint32_t, 1>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint32_t>);
    ASSERT_EQ(value, 0b10101111_u32);
}

TEST_F(ToUInt32AsLE, TwoBytes)
{
    auto value = bits::to_ule<std::uint32_t, 2>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint32_t>);
    ASSERT_EQ(value, 0b00010111'10101111_u32);
}

TEST_F(ToUInt32AsLE, ThreeBytes)
{
    auto value = bits::to_ule<std::uint32_t, 3>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint32_t>);
    ASSERT_EQ(value, 0b00001111'00010111'10101111_u32);
}

TEST_F(ToUInt32AsLE, FourBytes)
{
    auto value = bits::to_ule<std::uint32_t, 4>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint32_t>);
    ASSERT_EQ(value, 0b00110011'00001111'00010111'10101111_u32);
}

TEST_F(ToUInt64AsLE, OneByte)
{
    auto value = bits::to_ule<std::uint64_t, 1>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint64_t>);
    ASSERT_EQ(value, 0b10101111_u64);
}

TEST_F(ToUInt64AsLE, TwoBytes)
{
    auto value = bits::to_ule<std::uint64_t, 2>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint64_t>);
    ASSERT_EQ(value, 0b00010111'10101111_u64);
}

TEST_F(ToUInt64AsLE, ThreeBytes)
{
    auto value = bits::to_ule<std::uint64_t, 3>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint64_t>);
    ASSERT_EQ(value, 0b00001111'00010111'10101111_u64);
}

TEST_F(ToUInt64AsLE, FourBytes)
{
    auto value = bits::to_ule<std::uint64_t, 4>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint64_t>);
    ASSERT_EQ(value, 0b00110011'00001111'00010111'10101111_u64);
}

TEST_F(ToUInt64AsLE, FiveBytes)
{
    auto value = bits::to_ule<std::uint64_t, 5>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint64_t>);
    ASSERT_EQ(value, 0b10101010'00110011'00001111'00010111'10101111_u64);
}

TEST_F(ToUInt64AsLE, SixBytes)
{
    auto value = bits::to_ule<std::uint64_t, 6>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint64_t>);
    ASSERT_EQ(value,
              0b01111110'10101010'00110011'00001111'00010111'10101111_u64);
}

TEST_F(ToUInt64AsLE, SevenBytes)
{
    auto value = bits::to_ule<std::uint64_t, 7>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint64_t>);
    ASSERT_EQ(
        value,
        0b10000001'01111110'10101010'00110011'00001111'00010111'10101111_u64);
}

TEST_F(ToUInt64AsLE, EightBytes)
{
    auto value = bits::to_ule<std::uint64_t, 8>(Src{src_.data()});
    static_assert(std::is_same_v<decltype(value), std::uint64_t>);
    ASSERT_EQ(
        value,
        0b10011001'10000001'01111110'10101010'00110011'00001111'00010111'10101111_u64);
}
}  // namespace
