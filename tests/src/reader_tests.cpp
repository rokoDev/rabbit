#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include "serialization_tests.h"

using Reader = Data<std::byte, 64>;
using reader = rabbit::reader<rabbit::core, rabbit::tag_t,
                              rabbit::reader_error_result_adapter>;

TEST_F(Reader, DeserializeUInt8)
{
    rawBuf_[0] = 0b10100111_b;
    reader r(rawBuf_);
    std::uint8_t v = r.getValue<std::uint8_t>(NumBits(3));
    ASSERT_EQ(v, 0b00000101);
    ASSERT_EQ(r.pos(), bit_pos(3));
}

TEST_F(Reader, Deserialize3UInt8)
{
    rawBuf_[0] = 0b10100111_b;
    rawBuf_[1] = 0b10111101_b;
    rawBuf_[2] = 0b00000000_b;

    reader r(rawBuf_);

    std::uint8_t val1 = r.getValue<std::uint8_t>(NumBits(3));
    ASSERT_EQ(val1, 0b00000101);

    std::uint8_t val2 = r.getValue<std::uint8_t>(NumBits(5));
    ASSERT_EQ(val2, 0b00000111);

    std::uint8_t val3 = r.getValue<std::uint8_t>(NumBits(5));
    ASSERT_EQ(val3, 0b00010111);

    ASSERT_EQ(r.pos(), bit_pos(13));
    ASSERT_EQ(r.bytes_used(), 2_uz);
}

TEST_F(Reader, DeserializeCharBitsOneByOne)
{
    rawBuf_[0] = 0b11111111_b;

    reader r(rawBuf_);

    for (std::size_t i = 0; i < CHAR_BIT; ++i)
    {
        std::uint8_t val = r.getValue<std::uint8_t>(NumBits(1));
        ASSERT_EQ(val, 1_u8);
    }
}

TEST_F(Reader, Deserialize21BitsOfUInt32)
{
    rawBuf_[0] = 0b00000101_b;
    rawBuf_[1] = 0b00011100_b;
    rawBuf_[2] = 0b10101101_b;

    std::byte deserialized[3]{};

    reader r(Src(rawBuf_), n_bytes(3));

    r.getBits(deserialized, NumBits(21));

    ASSERT_EQ(deserialized[0], 0b00000101_b);
    ASSERT_EQ(deserialized[1], 0b00011100_b);
    ASSERT_EQ(deserialized[2], 0b10101000_b);
    ASSERT_EQ(r.pos(), bit_pos(21));
    ASSERT_EQ(r.pos().bitOffset(), 5);
    ASSERT_EQ(r.pos().byteIndex(), 2_uz);
}

TEST_F(Reader, Deserialize21BitsOfUInt32By1Offset)
{
    rawBuf_[0] = 0b00000101_b;
    rawBuf_[1] = 0b00011100_b;
    rawBuf_[2] = 0b10101101_b;

    std::byte deserialized[3]{};

    auto r = reader(Src(rawBuf_), n_bytes(3), bit_pos(1));
    r.getBits(deserialized, NumBits(21));

    ASSERT_EQ(deserialized[0], 0b00001010_b);
    ASSERT_EQ(deserialized[1], 0b00111001_b);
    ASSERT_EQ(deserialized[2], 0b01011000_b);
    ASSERT_EQ(r.pos(), bit_pos(22));
    ASSERT_EQ(r.pos().bitOffset(), 6);
    ASSERT_EQ(r.pos().byteIndex(), 2_uz);
}
