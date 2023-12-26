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

TEST_F(SimpleBinWriter, SerializeUInt8)
{
    constexpr std::uint8_t value = 0b00000101;
    rabbit::simple_writer swriter(rawBuf_);
    swriter.addValue(value, NumBits(3));
    ASSERT_EQ(swriter[n_bytes(0)], 0b10100000_b);
}

TEST_F(SimpleBinWriter, Serialize3UInt8)
{
    rabbit::simple_writer swriter(rawBuf_);

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
    rabbit::simple_writer swriter(Dst(rawBuf_), n_bytes(1));
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
    rabbit::simple_writer swriter(Dst(rawBuf_), n_bytes(3));

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
            BOOST_LEAF_AUTO(bwriter,
                            rabbit::make_bin_writer(Dst(rawBuf_), n_bytes(1)));
            std::uint16_t someValue{10};
            BOOST_LEAF_CHECK(bwriter.addValue(someValue));
            return {};
        });
}

TEST_F(BinWriter, AddValueNumBitsExceedTypeSize)
{
    expectError(writer_error::num_bits_exceed_type_size);
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(bwriter,
                            rabbit::make_bin_writer(Dst(rawBuf_), n_bytes(3)));
            using ValueT = std::uint16_t;
            ValueT someValue{10};
            BOOST_LEAF_CHECK(bwriter.addValue(
                someValue, NumBits(utils::num_bits<ValueT>() + 1)));
            return {};
        });
}

TEST_F(BinWriter, SerializeUInt8)
{
    constexpr std::uint8_t value = 0b00000101;
    auto bwriter = rabbit::make_bin_writer(rawBuf_);
    ASSERT_TRUE(bwriter.has_value());
    std::ignore = bwriter->addValue(value, NumBits(3));
    ASSERT_EQ(*((*bwriter)[n_bytes(0)]), 0b10100000_b);
}

TEST_F(BinWriter, Serialize3UInt8)
{
    auto bwriter = rabbit::make_bin_writer(rawBuf_);
    ASSERT_TRUE(bwriter.has_value());

    constexpr std::uint8_t value1 = 0b00000101;
    std::ignore = bwriter->addValue(value1, NumBits(3));
    ASSERT_EQ(*((*bwriter)[n_bytes(0)]), 0b10100000_b);

    constexpr std::uint8_t value2 = 0b00000111;
    std::ignore = bwriter->addValue(value2, NumBits(5));
    ASSERT_EQ(*((*bwriter)[n_bytes(0)]), 0b10100111_b);

    constexpr std::uint8_t value3 = 0b00010111;
    std::ignore = bwriter->addValue(value3, NumBits(5));
    ASSERT_EQ(*((*bwriter)[n_bytes(0)]), 0b10100111_b);
    ASSERT_EQ(*((*bwriter)[n_bytes(1)]), 0b10111000_b);
}

TEST_F(BinWriter, SerializeCharBitsOneByOne)
{
    auto bwriter = rabbit::make_bin_writer(Dst(rawBuf_), n_bytes(1));
    ASSERT_TRUE(bwriter.has_value());
    ASSERT_EQ(bwriter->pos(), bit_pos(0));
    ASSERT_EQ(bwriter->buffer().size(), n_bytes(1));

    execute(
        [&]() -> result<void>
        {
            constexpr std::uint8_t kValue = 0b00000001;
            for (std::size_t i = 0; i < CHAR_BIT; ++i)
            {
                BOOST_LEAF_CHECK(bwriter->addValue(kValue, NumBits(1)));
            }
            return {};
        });

    ASSERT_EQ(*((*bwriter)[n_bytes(0)]), 0b11111111_b);
    ASSERT_EQ(bwriter->pos(), bit_pos(CHAR_BIT));
    ASSERT_EQ(rawBuf_[1], 0_b);
}

TEST_F(BinWriter, SerializeCharBitsPlus1OneByOne)
{
    auto bwriter = rabbit::make_bin_writer(Dst(rawBuf_), n_bytes(1));
    expectError(writer_error::not_enough_buffer_size);
    execute(
        [&]() -> result<void>
        {
            constexpr std::uint8_t kValue = 0b00000001;
            for (std::size_t i = 0; i < CHAR_BIT + 1; ++i)
            {
                BOOST_LEAF_CHECK(bwriter->addValue(kValue, NumBits(1)));
            }
            return {};
        });

    ASSERT_EQ(*((*bwriter)[n_bytes(0)]), 0b11111111_b);
    ASSERT_EQ(bwriter->pos(), bit_pos(CHAR_BIT));
    ASSERT_EQ(rawBuf_[1], 0_b);
}

TEST_F(BinWriter, Serialize21BitsOfUInt32)
{
    auto bwriter = rabbit::make_bin_writer(Dst(rawBuf_), n_bytes(3));
    execute(
        [&]() -> result<void>
        {
            constexpr std::byte arr[] = {0b00000101_b, 0b00011100_b,
                                         0b10101010_b, 0b11001100_b};
            BOOST_LEAF_CHECK(bwriter->addBits(Src(arr), NumBits(21)));
            return {};
        });

    ASSERT_EQ(rawBuf_[0], 0b00000101_b);
    ASSERT_EQ(rawBuf_[1], 0b00011100_b);
    ASSERT_EQ(rawBuf_[2], 0b10101000_b);
    ASSERT_EQ(bwriter->pos(), bit_pos(21));
    ASSERT_EQ(bwriter->pos().bitOffset(), 5);
    ASSERT_EQ(bwriter->pos().byteIndex(), 2_uz);
}

TEST_F(BinWriter, SerializeMoreBitsThanContainsInSource)
{
    auto bwriter = rabbit::make_bin_writer(Dst(rawBuf_), n_bytes(5));
    expectError(writer_error::write_more_than_source_size);
    execute(
        [&]() -> result<void>
        {
            const std::byte arr[] = {0b00000101_b, 0b00011100_b, 0b10101010_b,
                                     0b11001100_b};
            BOOST_LEAF_CHECK(bwriter->addBits(arr, NumBits(5 * CHAR_BIT)));
            return {};
        });

    ASSERT_EQ(rawBuf_[0], 0b00000000_b);
    ASSERT_EQ(rawBuf_[1], 0b00000000_b);
    ASSERT_EQ(rawBuf_[2], 0b00000000_b);
    ASSERT_EQ(bwriter->pos(), bit_pos(0));
    ASSERT_EQ(bwriter->pos().bitOffset(), 0);
    ASSERT_EQ(bwriter->pos().byteIndex(), 0_uz);
}
