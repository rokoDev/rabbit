#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include "serialization_tests.h"

using BinReader = Data<std::byte, 64>;
using SimpleBinReader = Data<std::byte, 64>;

TEST_F(SimpleBinReader, DeserializeUInt8)
{
    rawBuf_[0] = 0b10100111_b;
    rabbit::simple_reader r(rawBuf_);
    std::uint8_t v{};
    const auto error = r.getValue(v, NumBits(3));
    ASSERT_EQ(error, rabbit::eReaderError::kSuccess);
    ASSERT_EQ(v, 0b00000101);
    ASSERT_EQ(r.pos(), bit_pos(3));
}

TEST_F(SimpleBinReader, Deserialize3UInt8)
{
    rawBuf_[0] = 0b10100111_b;
    rawBuf_[1] = 0b10111101_b;
    rawBuf_[2] = 0b00000000_b;

    rabbit::simple_reader sreader(rawBuf_);

    std::uint8_t val1{}, val2{}, val3{};

    ASSERT_EQ(sreader.getValue(val1, NumBits(3)),
              rabbit::eReaderError::kSuccess);
    ASSERT_EQ(val1, 0b00000101);

    ASSERT_EQ(sreader.getValue(val2, NumBits(5)),
              rabbit::eReaderError::kSuccess);
    ASSERT_EQ(val2, 0b00000111);

    ASSERT_EQ(sreader.getValue(val3, NumBits(5)),
              rabbit::eReaderError::kSuccess);
    ASSERT_EQ(val3, 0b00010111);

    ASSERT_EQ(sreader.pos(), bit_pos(13));
    ASSERT_EQ(sreader.bytes_used(), 2_uz);
}

TEST_F(SimpleBinReader, DeserializeCharBitsOneByOne)
{
    rawBuf_[0] = 0b11111111_b;

    rabbit::simple_reader sreader(rawBuf_);

    for (std::size_t i = 0; i < CHAR_BIT; ++i)
    {
        std::uint8_t val{};
        ASSERT_EQ(sreader.getValue(val, NumBits(1)),
                  rabbit::eReaderError::kSuccess);
        ASSERT_EQ(val, 1_u8);
    }
}

TEST_F(SimpleBinReader, Deserialize21BitsOfUInt32)
{
    rawBuf_[0] = 0b00000101_b;
    rawBuf_[1] = 0b00011100_b;
    rawBuf_[2] = 0b10101101_b;

    std::byte deserialized[3]{};

    rabbit::simple_reader sreader(Src(rawBuf_), n_bytes(3));

    ASSERT_EQ(sreader.getBits(deserialized, NumBits(21)),
              rabbit::eReaderError::kSuccess);

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

    auto breader =
        rabbit::make_bin_reader(Src(rawBuf_), n_bytes(3), bit_pos(1));
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_CHECK(breader->getBits(deserialized, NumBits(21)));
            return {};
        });

    ASSERT_EQ(deserialized[0], 0b00001010_b);
    ASSERT_EQ(deserialized[1], 0b00111001_b);
    ASSERT_EQ(deserialized[2], 0b01011000_b);
    ASSERT_EQ(breader->pos(), bit_pos(22));
    ASSERT_EQ(breader->pos().bitOffset(), 6);
    ASSERT_EQ(breader->pos().byteIndex(), 2_uz);
}

TEST_F(BinReader, ConstructFail1)
{
    expectError(buffer_error::null_data);
    execute(
        []() -> result<void>
        {
            BOOST_LEAF_CHECK(rabbit::make_bin_reader(Src(nullptr), n_bytes(5)));
            return {};
        });
}

TEST_F(BinReader, ConstructFail2)
{
    expectError(buffer_error::null_data_and_zero_size);
    execute(
        []() -> result<void>
        {
            BOOST_LEAF_CHECK(rabbit::make_bin_reader(Src(nullptr), n_bytes(0)));
            return {};
        });
}

TEST_F(BinReader, ConstructFail3)
{
    expectError(buffer_error::zero_size);
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_CHECK(rabbit::make_bin_reader(Src(rawBuf_), n_bytes(0)));
            return {};
        });
}

TEST_F(BinReader, ConstructFail4)
{
    expectError(reader_error::invalid_start_pos);
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_CHECK(
                rabbit::make_bin_reader(Src(rawBuf_), n_bytes(1), bit_pos(8)));
            return {};
        });
}

TEST_F(BinReader, GetValueNotEnoughBufferSize)
{
    expectError(reader_error::not_enough_buffer_size);
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(breader,
                            rabbit::make_bin_reader(Src(rawBuf_), n_bytes(1)));
            BOOST_LEAF_CHECK(breader.getValue<std::uint16_t>());
            return {};
        });
}

TEST_F(BinReader, GetValueNumBitsExceedTypeSize)
{
    expectError(reader_error::num_bits_exceed_type_size);
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(breader,
                            rabbit::make_bin_reader(Src(rawBuf_), n_bytes(3)));
            using ValueT = std::uint16_t;
            BOOST_LEAF_CHECK(breader.getValue<ValueT>(
                NumBits(utils::num_bits<ValueT>() + 1)));
            return {};
        });
}

TEST_F(BinReader, DeserializeUInt8)
{
    rawBuf_[0] = 0b10100111_b;
    auto breader = rabbit::make_bin_reader(rawBuf_);
    ASSERT_TRUE(breader.has_value());
    auto v = breader->getValue<std::uint8_t>(NumBits(3));
    ASSERT_TRUE(v.has_value());
    ASSERT_EQ(*v, 0b00000101);
    ASSERT_EQ(breader->pos(), bit_pos(3));
}

TEST_F(BinReader, Deserialize3UInt8)
{
    rawBuf_[0] = 0b10100111_b;
    rawBuf_[1] = 0b10111101_b;
    rawBuf_[2] = 0b00000000_b;

    std::uint8_t val1{}, val2{}, val3{};

    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(breader, rabbit::make_bin_reader(rawBuf_));

            BOOST_LEAF_AUTO(v1, breader.getValue<std::uint8_t>(NumBits(3)));
            val1 = v1;

            BOOST_LEAF_AUTO(v2, breader.getValue<std::uint8_t>(NumBits(5)));
            val2 = v2;

            BOOST_LEAF_AUTO(v3, breader.getValue<std::uint8_t>(NumBits(5)));
            val3 = v3;

            return {};
        });

    ASSERT_EQ(val1, 0b00000101);
    ASSERT_EQ(val2, 0b00000111);
    ASSERT_EQ(val3, 0b00010111);
}

TEST_F(BinReader, DeserializeCharBitsOneByOne)
{
    rawBuf_[0] = 0b11111111_b;
    std::uint8_t vals[CHAR_BIT]{};

    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(breader,
                            rabbit::make_bin_reader(Src(rawBuf_), n_bytes(1)));
            for (std::size_t i = 0; i < CHAR_BIT; ++i)
            {
                BOOST_LEAF_AUTO(val,
                                breader.getValue<std::uint8_t>(NumBits(1)));
                vals[i] = val;
            }
            return {};
        });

    for (std::size_t i = 0; i < CHAR_BIT; ++i)
    {
        ASSERT_EQ(vals[i], 1);
    }
}

TEST_F(BinReader, DeserializeCharBitsPlus1OneByOne)
{
    rawBuf_[0] = 0b11111111_b;
    rawBuf_[1] = 0b11110000_b;
    auto breader = rabbit::make_bin_reader(Src(rawBuf_), n_bytes(1));
    expectError(reader_error::not_enough_buffer_size);
    execute(
        [&]() -> result<void>
        {
            for (std::size_t i = 0; i < CHAR_BIT + 1; ++i)
            {
                BOOST_LEAF_CHECK(breader->getValue<std::uint8_t>(NumBits(1)));
            }
            return {};
        });

    ASSERT_EQ(breader->pos(), bit_pos(CHAR_BIT));
}

TEST_F(BinReader, Deserialize21BitsOfUInt32)
{
    rawBuf_[0] = 0b00000101_b;
    rawBuf_[1] = 0b00011100_b;
    rawBuf_[2] = 0b10101101_b;

    std::byte deserialized[3]{};

    auto breader = rabbit::make_bin_reader(Src(rawBuf_), n_bytes(3));
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_CHECK(breader->getBits(deserialized, NumBits(21)));
            return {};
        });

    ASSERT_EQ(deserialized[0], 0b00000101_b);
    ASSERT_EQ(deserialized[1], 0b00011100_b);
    ASSERT_EQ(deserialized[2], 0b10101000_b);
    ASSERT_EQ(breader->pos(), bit_pos(21));
    ASSERT_EQ(breader->pos().bitOffset(), 5);
    ASSERT_EQ(breader->pos().byteIndex(), 2_uz);
}

TEST_F(BinReader, Deserialize21BitsOfUInt32By1Offset)
{
    rawBuf_[0] = 0b00000101_b;
    rawBuf_[1] = 0b00011100_b;
    rawBuf_[2] = 0b10101101_b;

    std::byte deserialized[3]{};

    auto breader =
        rabbit::make_bin_reader(Src(rawBuf_), n_bytes(3), bit_pos(1));
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_CHECK(breader->getBits(deserialized, NumBits(21)));
            return {};
        });

    ASSERT_EQ(deserialized[0], 0b00001010_b);
    ASSERT_EQ(deserialized[1], 0b00111001_b);
    ASSERT_EQ(deserialized[2], 0b01011000_b);
    ASSERT_EQ(breader->pos(), bit_pos(22));
    ASSERT_EQ(breader->pos().bitOffset(), 6);
    ASSERT_EQ(breader->pos().byteIndex(), 2_uz);
}

TEST_F(BinReader, DeserializeMoreBitsThanContainsInDestination)
{
    auto breader = rabbit::make_bin_reader(Src(rawBuf_), n_bytes(5));
    expectError(reader_error::read_more_than_destination_size);
    execute(
        [&]() -> result<void>
        {
            std::byte arr[4]{};
            BOOST_LEAF_CHECK(breader->getBits(arr, NumBits(5 * CHAR_BIT)));
            return {};
        });

    ASSERT_EQ(breader->pos(), bit_pos(0));
    ASSERT_EQ(breader->pos().bitOffset(), 0);
    ASSERT_EQ(breader->pos().byteIndex(), 0_uz);
}
