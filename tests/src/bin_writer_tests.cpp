#include <buffer/buffer.h>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include <memory>

#include "serialization_tests.h"

using BinWriter = Data<uint8_t, 64>;

using SimpleBinWriter = Data<uint8_t, 64>;

TEST_F(SimpleBinWriter, SerializeUInt8)
{
    constexpr uint8_t value = 0b00000101;
    rabbit::simple_writer writer(rawBuf_);
    writer.addValue(value, NumBits(3));
    ASSERT_EQ(writer[n_bytes(0)], 0b10100000);
}

TEST_F(SimpleBinWriter, Serialize3UInt8)
{
    rabbit::simple_writer writer(rawBuf_);

    constexpr uint8_t value1 = 0b00000101;
    writer.addValue(value1, NumBits(3));
    ASSERT_EQ(writer[n_bytes(0)], 0b10100000);

    constexpr uint8_t value2 = 0b00000111;
    writer.addValue(value2, NumBits(5));
    ASSERT_EQ(writer[n_bytes(0)], 0b10100111);

    constexpr uint8_t value3 = 0b00010111;
    writer.addValue(value3, NumBits(5));
    ASSERT_EQ(writer[n_bytes(0)], 0b10100111);
    ASSERT_EQ(writer[n_bytes(1)], 0b10111000);
}

TEST_F(SimpleBinWriter, SerializeCharBitsOneByOne)
{
    rabbit::simple_writer writer(Dst(rawBuf_), n_bytes(1));
    ASSERT_EQ(writer.pos(), bit_pos(0));
    ASSERT_EQ(writer.buffer().size(), n_bytes(1));

    constexpr uint8_t kValue = 0b00000001;
    for (std::size_t i = 0; i < CHAR_BIT; ++i)
    {
        writer.addValue(kValue, NumBits(1));
    }

    ASSERT_EQ(writer[n_bytes(0)], 0b11111111);
    ASSERT_EQ(writer.pos(), bit_pos(CHAR_BIT));
    ASSERT_EQ(rawBuf_[1], 0);
}

TEST_F(SimpleBinWriter, Serialize21BitsOfUInt32)
{
    rabbit::simple_writer writer(Dst(rawBuf_), n_bytes(3));

    constexpr uint8_t arr[] = {0b00000101, 0b00011100, 0b10101010, 0b11001100};
    writer.addBits(Src(arr), NumBits(21));

    ASSERT_EQ(rawBuf_[0], 0b00000101);
    ASSERT_EQ(rawBuf_[1], 0b00011100);
    ASSERT_EQ(rawBuf_[2], 0b10101000);
    ASSERT_EQ(writer.pos(), bit_pos(21));
    ASSERT_EQ(writer.pos().bitOffset(), 5);
    ASSERT_EQ(writer.pos().byteIndex(), 2_uz);
}

TEST_F(BinWriter, ConstructFail1)
{
    expectError(buffer_error::null_data);
    execute(
        []() -> result<void>
        {
            BOOST_LEAF_CHECK(rabbit::make_bin_writer(Dst(nullptr), n_bytes(5)));
            return {};
        });
}

TEST_F(BinWriter, ConstructFail2)
{
    expectError(buffer_error::null_data_and_zero_size);
    execute(
        []() -> result<void>
        {
            BOOST_LEAF_CHECK(rabbit::make_bin_writer(Dst(nullptr), n_bytes(0)));
            return {};
        });
}

TEST_F(BinWriter, ConstructFail3)
{
    expectError(buffer_error::zero_size);
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_CHECK(rabbit::make_bin_writer(Dst(rawBuf_), n_bytes(0)));
            return {};
        });
}

TEST_F(BinWriter, ConstructFail4)
{
    expectError(writer_error::invalid_start_pos);
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_CHECK(
                rabbit::make_bin_writer(Dst(rawBuf_), n_bytes(1), bit_pos(8)));
            return {};
        });
}

TEST_F(BinWriter, AddValueNotEnoughBufferSize)
{
    expectError(writer_error::not_enough_buffer_size);
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(writer,
                            rabbit::make_bin_writer(Dst(rawBuf_), n_bytes(1)));
            uint16_t someValue{10};
            BOOST_LEAF_CHECK(writer.addValue(someValue));
            return {};
        });
}

TEST_F(BinWriter, AddValueNumBitsExceedTypeSize)
{
    expectError(writer_error::num_bits_exceed_type_size);
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(writer,
                            rabbit::make_bin_writer(Dst(rawBuf_), n_bytes(3)));
            using ValueT = uint16_t;
            ValueT someValue{10};
            BOOST_LEAF_CHECK(writer.addValue(
                someValue, NumBits(utils::num_bits<ValueT>() + 1)));
            return {};
        });
}

TEST_F(BinWriter, SerializeUInt8)
{
    constexpr uint8_t value = 0b00000101;
    auto writer = rabbit::make_bin_writer(rawBuf_);
    ASSERT_TRUE(writer.has_value());
    writer->addValue(value, NumBits(3));
    ASSERT_EQ(*((*writer)[n_bytes(0)]), 0b10100000);
}

TEST_F(BinWriter, Serialize3UInt8)
{
    auto writer = rabbit::make_bin_writer(rawBuf_);
    ASSERT_TRUE(writer.has_value());

    constexpr uint8_t value1 = 0b00000101;
    writer->addValue(value1, NumBits(3));
    ASSERT_EQ(*((*writer)[n_bytes(0)]), 0b10100000);

    constexpr uint8_t value2 = 0b00000111;
    writer->addValue(value2, NumBits(5));
    ASSERT_EQ(*((*writer)[n_bytes(0)]), 0b10100111);

    constexpr uint8_t value3 = 0b00010111;
    writer->addValue(value3, NumBits(5));
    ASSERT_EQ(*((*writer)[n_bytes(0)]), 0b10100111);
    ASSERT_EQ(*((*writer)[n_bytes(1)]), 0b10111000);
}

TEST_F(BinWriter, SerializeCharBitsOneByOne)
{
    auto writer = rabbit::make_bin_writer(Dst(rawBuf_), n_bytes(1));
    ASSERT_TRUE(writer.has_value());
    ASSERT_EQ(writer->pos(), bit_pos(0));
    ASSERT_EQ(writer->buffer().size(), n_bytes(1));

    execute(
        [&]() -> result<void>
        {
            constexpr uint8_t kValue = 0b00000001;
            for (std::size_t i = 0; i < CHAR_BIT; ++i)
            {
                BOOST_LEAF_CHECK(writer->addValue(kValue, NumBits(1)));
            }
            return {};
        });

    ASSERT_EQ(*((*writer)[n_bytes(0)]), 0b11111111);
    ASSERT_EQ(writer->pos(), bit_pos(CHAR_BIT));
    ASSERT_EQ(rawBuf_[1], 0);
}

TEST_F(BinWriter, SerializeCharBitsPlus1OneByOne)
{
    auto writer = rabbit::make_bin_writer(Dst(rawBuf_), n_bytes(1));
    expectError(writer_error::not_enough_buffer_size);
    execute(
        [&]() -> result<void>
        {
            constexpr uint8_t kValue = 0b00000001;
            for (std::size_t i = 0; i < CHAR_BIT + 1; ++i)
            {
                BOOST_LEAF_CHECK(writer->addValue(kValue, NumBits(1)));
            }
            return {};
        });

    ASSERT_EQ(*((*writer)[n_bytes(0)]), 0b11111111);
    ASSERT_EQ(writer->pos(), bit_pos(CHAR_BIT));
    ASSERT_EQ(rawBuf_[1], 0);
}

TEST_F(BinWriter, Serialize21BitsOfUInt32)
{
    auto writer = rabbit::make_bin_writer(Dst(rawBuf_), n_bytes(3));
    execute(
        [&]() -> result<void>
        {
            constexpr uint8_t arr[] = {0b00000101, 0b00011100, 0b10101010,
                                       0b11001100};
            BOOST_LEAF_CHECK(writer->addBits(Src(arr), NumBits(21)));
            return {};
        });

    ASSERT_EQ(rawBuf_[0], 0b00000101);
    ASSERT_EQ(rawBuf_[1], 0b00011100);
    ASSERT_EQ(rawBuf_[2], 0b10101000);
    ASSERT_EQ(writer->pos(), bit_pos(21));
    ASSERT_EQ(writer->pos().bitOffset(), 5);
    ASSERT_EQ(writer->pos().byteIndex(), 2_uz);
}

TEST_F(BinWriter, SerializeMoreBitsThanContainsInSource)
{
    auto writer = rabbit::make_bin_writer(Dst(rawBuf_), n_bytes(5));
    expectError(writer_error::write_more_than_source_size);
    execute(
        [&]() -> result<void>
        {
            const uint8_t arr[] = {0b00000101, 0b00011100, 0b10101010,
                                   0b11001100};
            BOOST_LEAF_CHECK(writer->addBits(arr, NumBits(5 * CHAR_BIT)));
            return {};
        });

    ASSERT_EQ(rawBuf_[0], 0b00000000);
    ASSERT_EQ(rawBuf_[1], 0b00000000);
    ASSERT_EQ(rawBuf_[2], 0b00000000);
    ASSERT_EQ(writer->pos(), bit_pos(0));
    ASSERT_EQ(writer->pos().bitOffset(), 0);
    ASSERT_EQ(writer->pos().byteIndex(), 0_uz);
}
