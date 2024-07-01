#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include "serialization_tests.h"

using BinReader = Data<std::byte, 64>;
using SimpleBinReader = Data<std::byte, 64>;
using simple_reader =
    rabbit::simple_bin_reader<rabbit::core, rabbit::tag_t,
                              rabbit::reader_error_result_adapter>;

template <typename T>
using result = boost::leaf::result<T>;

TEST_F(SimpleBinReader, DeserializeUInt8)
{
    rawBuf_[0] = 0b10100111_b;
    simple_reader r(rawBuf_);
    std::uint8_t v = r.getValue<std::uint8_t>(NumBits(3));
    ASSERT_EQ(v, 0b00000101);
    ASSERT_EQ(r.pos(), bit_pos(3));
}

TEST_F(SimpleBinReader, Deserialize3UInt8)
{
    rawBuf_[0] = 0b10100111_b;
    rawBuf_[1] = 0b10111101_b;
    rawBuf_[2] = 0b00000000_b;

    simple_reader sreader(rawBuf_);

    std::uint8_t val1 = sreader.getValue<std::uint8_t>(NumBits(3));
    ASSERT_EQ(val1, 0b00000101);

    std::uint8_t val2 = sreader.getValue<std::uint8_t>(NumBits(5));
    ASSERT_EQ(val2, 0b00000111);

    std::uint8_t val3 = sreader.getValue<std::uint8_t>(NumBits(5));
    ASSERT_EQ(val3, 0b00010111);

    ASSERT_EQ(sreader.pos(), bit_pos(13));
    ASSERT_EQ(sreader.bytes_used(), 2_uz);
}

TEST_F(SimpleBinReader, DeserializeCharBitsOneByOne)
{
    rawBuf_[0] = 0b11111111_b;

    simple_reader sreader(rawBuf_);

    for (std::size_t i = 0; i < CHAR_BIT; ++i)
    {
        std::uint8_t val = sreader.getValue<std::uint8_t>(NumBits(1));
        ASSERT_EQ(val, 1_u8);
    }
}

TEST_F(SimpleBinReader, Deserialize21BitsOfUInt32)
{
    rawBuf_[0] = 0b00000101_b;
    rawBuf_[1] = 0b00011100_b;
    rawBuf_[2] = 0b10101101_b;

    std::byte deserialized[3]{};

    simple_reader sreader(Src(rawBuf_), n_bytes(3));

    sreader.getBits(deserialized, NumBits(21));

    ASSERT_EQ(deserialized[0], 0b00000101_b);
    ASSERT_EQ(deserialized[1], 0b00011100_b);
    ASSERT_EQ(deserialized[2], 0b10101000_b);
    ASSERT_EQ(sreader.pos(), bit_pos(21));
    ASSERT_EQ(sreader.pos().bitOffset(), 5);
    ASSERT_EQ(sreader.pos().byteIndex(), 2_uz);
}

TEST_F(SimpleBinReader, Deserialize21BitsOfUInt32By1Offset)
{
    rawBuf_[0] = 0b00000101_b;
    rawBuf_[1] = 0b00011100_b;
    rawBuf_[2] = 0b10101101_b;

    std::byte deserialized[3]{};

    auto breader = simple_reader(Src(rawBuf_), n_bytes(3), bit_pos(1));
    breader.getBits(deserialized, NumBits(21));

    ASSERT_EQ(deserialized[0], 0b00001010_b);
    ASSERT_EQ(deserialized[1], 0b00111001_b);
    ASSERT_EQ(deserialized[2], 0b01011000_b);
    ASSERT_EQ(breader.pos(), bit_pos(22));
    ASSERT_EQ(breader.pos().bitOffset(), 6);
    ASSERT_EQ(breader.pos().byteIndex(), 2_uz);
}

TEST_F(BinReader, DeserializeUInt8)
{
    rawBuf_[0] = 0b10100111_b;
    auto breader = simple_reader(rawBuf_);
    auto v = breader.getValue<std::uint8_t>(NumBits(3));
    ASSERT_EQ(v, 0b00000101);
    ASSERT_EQ(breader.pos(), bit_pos(3));
}

TEST_F(BinReader, Deserialize3UInt8)
{
    rawBuf_[0] = 0b10100111_b;
    rawBuf_[1] = 0b10111101_b;
    rawBuf_[2] = 0b00000000_b;

    auto breader = simple_reader(rawBuf_);

    auto v1 = breader.getValue<std::uint8_t>(NumBits(3));
    auto v2 = breader.getValue<std::uint8_t>(NumBits(5));
    auto v3 = breader.getValue<std::uint8_t>(NumBits(5));

    ASSERT_EQ(v1, 0b00000101);
    ASSERT_EQ(v2, 0b00000111);
    ASSERT_EQ(v3, 0b00010111);
}

TEST_F(BinReader, DeserializeCharBitsOneByOne)
{
    rawBuf_[0] = 0b11111111_b;
    std::uint8_t vals[CHAR_BIT]{};

    auto breader = simple_reader(Src(rawBuf_), n_bytes(1));
    for (std::size_t i = 0; i < CHAR_BIT; ++i)
    {
        vals[i] = breader.getValue<std::uint8_t>(NumBits(1));
    }

    for (std::size_t i = 0; i < CHAR_BIT; ++i)
    {
        ASSERT_EQ(vals[i], 1);
    }
}

TEST_F(BinReader, Deserialize21BitsOfUInt32)
{
    rawBuf_[0] = 0b00000101_b;
    rawBuf_[1] = 0b00011100_b;
    rawBuf_[2] = 0b10101101_b;

    std::byte deserialized[3]{};

    auto breader = simple_reader(Src(rawBuf_), n_bytes(3));
    breader.getBits(deserialized, NumBits(21));

    ASSERT_EQ(deserialized[0], 0b00000101_b);
    ASSERT_EQ(deserialized[1], 0b00011100_b);
    ASSERT_EQ(deserialized[2], 0b10101000_b);
    ASSERT_EQ(breader.pos(), bit_pos(21));
    ASSERT_EQ(breader.pos().bitOffset(), 5);
    ASSERT_EQ(breader.pos().byteIndex(), 2_uz);
}

TEST_F(BinReader, Deserialize21BitsOfUInt32By1Offset)
{
    rawBuf_[0] = 0b00000101_b;
    rawBuf_[1] = 0b00011100_b;
    rawBuf_[2] = 0b10101101_b;

    std::byte deserialized[3]{};

    auto breader = simple_reader(Src(rawBuf_), n_bytes(3), bit_pos(1));
    breader.getBits(deserialized, NumBits(21));

    ASSERT_EQ(deserialized[0], 0b00001010_b);
    ASSERT_EQ(deserialized[1], 0b00111001_b);
    ASSERT_EQ(deserialized[2], 0b01011000_b);
    ASSERT_EQ(breader.pos(), bit_pos(22));
    ASSERT_EQ(breader.pos().bitOffset(), 6);
    ASSERT_EQ(breader.pos().byteIndex(), 2_uz);
}
