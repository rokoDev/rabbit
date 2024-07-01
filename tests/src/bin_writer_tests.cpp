#include <buffer/buffer.h>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include <memory>
#include <tuple>

#include "serialization_tests.h"

using BinWriter = Data<std::byte, 64>;
using SimpleBinWriter = Data<std::byte, 64>;
using simple_writer =
    rabbit::simple_bin_writer<rabbit::core, rabbit::tag_t,
                              rabbit::writer_error_result_adapter>;

TEST_F(SimpleBinWriter, SerializeUInt8)
{
    constexpr std::uint8_t value = 0b00000101;
    simple_writer swriter(rawBuf_);
    swriter.addValue(value, NumBits(3));
    ASSERT_EQ(swriter[n_bytes(0)], 0b10100000_b);
}

TEST_F(SimpleBinWriter, Serialize3UInt8)
{
    simple_writer swriter(rawBuf_);

    constexpr std::uint8_t value1 = 0b00000101;
    swriter.addValue(value1, NumBits(3));
    ASSERT_EQ(swriter[n_bytes(0)], 0b10100000_b);

    constexpr std::uint8_t value2 = 0b00000111;
    swriter.addValue(value2, NumBits(5));
    ASSERT_EQ(swriter[n_bytes(0)], 0b10100111_b);

    constexpr std::uint8_t value3 = 0b00010111;
    swriter.addValue(value3, NumBits(5));
    ASSERT_EQ(swriter[n_bytes(0)], 0b10100111_b);
    ASSERT_EQ(swriter[n_bytes(1)], 0b10111000_b);
}

TEST_F(SimpleBinWriter, SerializeCharBitsOneByOne)
{
    simple_writer swriter(Dst(rawBuf_), n_bytes(1));
    ASSERT_EQ(swriter.pos(), bit_pos(0));
    ASSERT_EQ(swriter.buffer().size(), n_bytes(1));

    constexpr std::uint8_t kValue = 0b00000001;
    for (std::size_t i = 0; i < CHAR_BIT; ++i)
    {
        swriter.addValue(kValue, NumBits(1));
    }

    ASSERT_EQ(swriter[n_bytes(0)], 0b11111111_b);
    ASSERT_EQ(swriter.pos(), bit_pos(CHAR_BIT));
    ASSERT_EQ(rawBuf_[1], 0_b);
}

TEST_F(SimpleBinWriter, Serialize21BitsOfUInt32)
{
    simple_writer swriter(Dst(rawBuf_), n_bytes(3));

    constexpr std::byte arr[] = {0b00000101_b, 0b00011100_b, 0b10101010_b,
                                 0b11001100_b};
    swriter.addBits(Src(arr), NumBits(21));

    ASSERT_EQ(rawBuf_[0], 0b00000101_b);
    ASSERT_EQ(rawBuf_[1], 0b00011100_b);
    ASSERT_EQ(rawBuf_[2], 0b10101000_b);
    ASSERT_EQ(swriter.pos(), bit_pos(21));
    ASSERT_EQ(swriter.pos().bitOffset(), 5);
    ASSERT_EQ(swriter.pos().byteIndex(), 2_uz);
}

TEST_F(BinWriter, SerializeUInt8)
{
    constexpr std::uint8_t value = 0b00000101;
    auto bwriter = simple_writer(rawBuf_);
    bwriter.addValue(value, NumBits(3));
    ASSERT_EQ(bwriter[n_bytes(0)], 0b10100000_b);
}

TEST_F(BinWriter, Serialize3UInt8)
{
    auto bwriter = simple_writer(rawBuf_);

    constexpr std::uint8_t value1 = 0b00000101;
    bwriter.addValue(value1, NumBits(3));
    ASSERT_EQ(bwriter[n_bytes(0)], 0b10100000_b);

    constexpr std::uint8_t value2 = 0b00000111;
    bwriter.addValue(value2, NumBits(5));
    ASSERT_EQ(bwriter[n_bytes(0)], 0b10100111_b);

    constexpr std::uint8_t value3 = 0b00010111;
    bwriter.addValue(value3, NumBits(5));
    ASSERT_EQ(bwriter[n_bytes(0)], 0b10100111_b);
    ASSERT_EQ(bwriter[n_bytes(1)], 0b10111000_b);
}

TEST_F(BinWriter, SerializeCharBitsOneByOne)
{
    auto bwriter = simple_writer(Dst(rawBuf_), n_bytes(1));
    ASSERT_EQ(bwriter.pos(), bit_pos(0));
    ASSERT_EQ(bwriter.buffer().size(), n_bytes(1));

    constexpr std::uint8_t kValue = 0b00000001;
    for (std::size_t i = 0; i < CHAR_BIT; ++i)
    {
        bwriter.addValue(kValue, NumBits(1));
    }

    ASSERT_EQ(bwriter[n_bytes(0)], 0b11111111_b);
    ASSERT_EQ(bwriter.pos(), bit_pos(CHAR_BIT));
    ASSERT_EQ(rawBuf_[1], 0_b);
}

TEST_F(BinWriter, Serialize21BitsOfUInt32)
{
    auto bwriter = simple_writer(Dst(rawBuf_), n_bytes(3));
    constexpr std::byte arr[] = {0b00000101_b, 0b00011100_b, 0b10101010_b,
                                 0b11001100_b};
    bwriter.addBits(Src(arr), NumBits(21));

    ASSERT_EQ(rawBuf_[0], 0b00000101_b);
    ASSERT_EQ(rawBuf_[1], 0b00011100_b);
    ASSERT_EQ(rawBuf_[2], 0b10101000_b);
    ASSERT_EQ(bwriter.pos(), bit_pos(21));
    ASSERT_EQ(bwriter.pos().bitOffset(), 5);
    ASSERT_EQ(bwriter.pos().byteIndex(), 2_uz);
}

TEST_F(BinWriter, Serialize21BitsOfUInt32By1Offset)
{
    rawBuf_[0] = 0b00000000_b;
    rawBuf_[1] = 0b00000000_b;
    rawBuf_[2] = 0b00000111_b;

    auto bwriter = simple_writer(Dst(rawBuf_), n_bytes(3), bit_pos(1));
    constexpr std::byte arr[] = {0b00000101_b, 0b00011100_b, 0b10101010_b,
                                 0b11001100_b};
    bwriter.addBits(arr, SrcOffset{10}, NumBits(21));

    ASSERT_EQ(rawBuf_[0], 0b00111001_b);
    ASSERT_EQ(rawBuf_[1], 0b01010101_b);
    ASSERT_EQ(rawBuf_[2], 0b10011011_b);
    ASSERT_EQ(bwriter.pos(), bit_pos(22));
    ASSERT_EQ(bwriter.pos().bitOffset(), 6);
    ASSERT_EQ(bwriter.pos().byteIndex(), 2_uz);
}
