#include <buffer/buffer.h>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include <memory>
#include <tuple>

#include "serialization_tests.h"

using Writer = Data<std::byte, 64>;
using writer = rabbit::writer<rabbit::core, rabbit::tag_t,
                              rabbit::writer_error_result_adapter>;

TEST_F(Writer, SerializeUInt8)
{
    constexpr std::uint8_t value = 0b00000101;
    writer w(rawBuf_);
    w.addValue(value, NumBits(3));
    ASSERT_EQ(w[n_bytes(0)], 0b10100000_b);
}

TEST_F(Writer, Serialize3UInt8)
{
    writer w(rawBuf_);

    constexpr std::uint8_t value1 = 0b00000101;
    w.addValue(value1, NumBits(3));
    ASSERT_EQ(w[n_bytes(0)], 0b10100000_b);

    constexpr std::uint8_t value2 = 0b00000111;
    w.addValue(value2, NumBits(5));
    ASSERT_EQ(w[n_bytes(0)], 0b10100111_b);

    constexpr std::uint8_t value3 = 0b00010111;
    w.addValue(value3, NumBits(5));
    ASSERT_EQ(w[n_bytes(0)], 0b10100111_b);
    ASSERT_EQ(w[n_bytes(1)], 0b10111000_b);
}

TEST_F(Writer, SerializeCharBitsOneByOne)
{
    writer w(Dst(rawBuf_), n_bytes(1));
    ASSERT_EQ(w.pos(), bit_pos(0));
    ASSERT_EQ(w.buffer().size(), n_bytes(1));

    constexpr std::uint8_t kValue = 0b00000001;
    for (std::size_t i = 0; i < CHAR_BIT; ++i)
    {
        w.addValue(kValue, NumBits(1));
    }

    ASSERT_EQ(w[n_bytes(0)], 0b11111111_b);
    ASSERT_EQ(w.pos(), bit_pos(CHAR_BIT));
    ASSERT_EQ(rawBuf_[1], 0_b);
}

TEST_F(Writer, Serialize21BitsOfUInt32)
{
    writer w(Dst(rawBuf_), n_bytes(3));

    constexpr std::byte arr[] = {0b00000101_b, 0b00011100_b, 0b10101010_b,
                                 0b11001100_b};
    w.addBits(Src(arr), NumBits(21));

    ASSERT_EQ(rawBuf_[0], 0b00000101_b);
    ASSERT_EQ(rawBuf_[1], 0b00011100_b);
    ASSERT_EQ(rawBuf_[2], 0b10101000_b);
    ASSERT_EQ(w.pos(), bit_pos(21));
    ASSERT_EQ(w.pos().bitOffset(), 5);
    ASSERT_EQ(w.pos().byteIndex(), 2_uz);
}

TEST_F(Writer, Serialize21BitsOfUInt32By1Offset)
{
    rawBuf_[0] = 0b00000000_b;
    rawBuf_[1] = 0b00000000_b;
    rawBuf_[2] = 0b00000111_b;

    auto w = writer(Dst(rawBuf_), n_bytes(3), bit_pos(1));
    constexpr std::byte arr[] = {0b00000101_b, 0b00011100_b, 0b10101010_b,
                                 0b11001100_b};
    w.addBits(arr, SrcOffset{10}, NumBits(21));

    ASSERT_EQ(rawBuf_[0], 0b00111001_b);
    ASSERT_EQ(rawBuf_[1], 0b01010101_b);
    ASSERT_EQ(rawBuf_[2], 0b10011011_b);
    ASSERT_EQ(w.pos(), bit_pos(22));
    ASSERT_EQ(w.pos().bitOffset(), 6);
    ASSERT_EQ(w.pos().byteIndex(), 2_uz);
}
