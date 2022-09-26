#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include "serialization_tests.h"

#ifdef BOOST_LEAF_NO_EXCEPTIONS

namespace boost
{
[[noreturn]] void throw_exception(std::exception const &e)
{
    std::cerr
        << "Terminating due to a C++ exception under BOOST_LEAF_NO_EXCEPTIONS: "
        << e.what();
    std::terminate();
}

struct source_location;
[[noreturn]] void throw_exception(std::exception const &e,
                                  boost::source_location const &)
{
    throw_exception(e);
}
}  // namespace boost

#endif

using BinReader = Data<uint8_t, 64>;

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
            BOOST_LEAF_AUTO(reader,
                            rabbit::make_bin_reader(Src(rawBuf_), n_bytes(1)));
            BOOST_LEAF_CHECK(reader.getValue<uint16_t>());
            return {};
        });
}

TEST_F(BinReader, GetValueNumBitsExceedTypeSize)
{
    expectError(reader_error::num_bits_exceed_type_size);
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(reader,
                            rabbit::make_bin_reader(Src(rawBuf_), n_bytes(3)));
            using ValueT = uint16_t;
            BOOST_LEAF_CHECK(reader.getValue<ValueT>(
                NumBits(utils::num_bits<ValueT>() + 1)));
            return {};
        });
}

TEST_F(BinReader, DeserializeUInt8)
{
    rawBuf_[0] = 0b10100111;
    auto reader = rabbit::make_bin_reader(rawBuf_);
    ASSERT_TRUE(reader.has_value());
    auto v = reader->getValue<uint8_t>(NumBits(3));
    ASSERT_TRUE(v.has_value());
    ASSERT_EQ(*v, 0b00000101);
    ASSERT_EQ(reader->pos(), bit_pos(3));
}

TEST_F(BinReader, Deserialize3UInt8)
{
    rawBuf_[0] = 0b10100111;
    rawBuf_[1] = 0b10111101;
    rawBuf_[2] = 0b00000000;

    uint8_t val1{}, val2{}, val3{};

    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(reader, rabbit::make_bin_reader(rawBuf_));

            BOOST_LEAF_AUTO(v1, reader.getValue<uint8_t>(NumBits(3)));
            val1 = v1;

            BOOST_LEAF_AUTO(v2, reader.getValue<uint8_t>(NumBits(5)));
            val2 = v2;

            BOOST_LEAF_AUTO(v3, reader.getValue<uint8_t>(NumBits(5)));
            val3 = v3;

            return {};
        });

    ASSERT_EQ(val1, 0b00000101);
    ASSERT_EQ(val2, 0b00000111);
    ASSERT_EQ(val3, 0b00010111);
}

TEST_F(BinReader, DeserializeCharBitsOneByOne)
{
    rawBuf_[0] = 0b11111111;
    uint8_t vals[CHAR_BIT]{};

    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(reader,
                            rabbit::make_bin_reader(Src(rawBuf_), n_bytes(1)));
            for (std::size_t i = 0; i < CHAR_BIT; ++i)
            {
                BOOST_LEAF_AUTO(val, reader.getValue<uint8_t>(NumBits(1)));
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
    rawBuf_[0] = 0b11111111;
    rawBuf_[1] = 0b11110000;
    auto reader = rabbit::make_bin_reader(Src(rawBuf_), n_bytes(1));
    expectError(reader_error::not_enough_buffer_size);
    execute(
        [&]() -> result<void>
        {
            for (std::size_t i = 0; i < CHAR_BIT + 1; ++i)
            {
                BOOST_LEAF_CHECK(reader->getValue<uint8_t>(NumBits(1)));
            }
            return {};
        });

    ASSERT_EQ(reader->pos(), bit_pos(CHAR_BIT));
}

TEST_F(BinReader, Deserialize21BitsOfUInt32)
{
    rawBuf_[0] = 0b00000101;
    rawBuf_[1] = 0b00011100;
    rawBuf_[2] = 0b10101101;

    uint8_t deserialized[3]{};

    auto reader = rabbit::make_bin_reader(Src(rawBuf_), n_bytes(3));
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_CHECK(reader->getBits(deserialized, NumBits(21)));
            return {};
        });

    ASSERT_EQ(deserialized[0], 0b00000101);
    ASSERT_EQ(deserialized[1], 0b00011100);
    ASSERT_EQ(deserialized[2], 0b10101000);
    ASSERT_EQ(reader->pos(), bit_pos(21));
    ASSERT_EQ(reader->pos().bitOffset(), 5);
    ASSERT_EQ(reader->pos().byteIndex(), 2_uz);
}

TEST_F(BinReader, Deserialize21BitsOfUInt32By1Offset)
{
    rawBuf_[0] = 0b00000101;
    rawBuf_[1] = 0b00011100;
    rawBuf_[2] = 0b10101101;

    uint8_t deserialized[3]{};

    auto reader = rabbit::make_bin_reader(Src(rawBuf_), n_bytes(3), bit_pos(1));
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_CHECK(reader->getBits(deserialized, NumBits(21)));
            return {};
        });

    ASSERT_EQ(deserialized[0], 0b00001010);
    ASSERT_EQ(deserialized[1], 0b00111001);
    ASSERT_EQ(deserialized[2], 0b01011000);
    ASSERT_EQ(reader->pos(), bit_pos(22));
    ASSERT_EQ(reader->pos().bitOffset(), 6);
    ASSERT_EQ(reader->pos().byteIndex(), 2_uz);
}

TEST_F(BinReader, DeserializeMoreBitsThanContainsInDestination)
{
    auto reader = rabbit::make_bin_reader(Src(rawBuf_), n_bytes(5));
    expectError(reader_error::read_more_than_destination_size);
    execute(
        [&]() -> result<void>
        {
            uint8_t arr[4]{};
            BOOST_LEAF_CHECK(reader->getBits(arr, NumBits(5 * CHAR_BIT)));
            return {};
        });

    ASSERT_EQ(reader->pos(), bit_pos(0));
    ASSERT_EQ(reader->pos().bitOffset(), 0);
    ASSERT_EQ(reader->pos().byteIndex(), 0_uz);
}
