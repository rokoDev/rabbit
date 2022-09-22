#include <buffer/buffer.h>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <rabbit/bin_writer.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include <limits>
#include <memory>

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

template <class T>
using result = boost::leaf::result<T>;

namespace leaf = boost::leaf;
using buffer_error = buffer::error;
using writer_error = rabbit::writer_error;
using Pos = buffer::bit_pos;
using n_bytes = buffer::n_bytes;
using NumBits = rabbit::NumBits;
using bit_pos = buffer::bit_pos;
using Dst = rabbit::Dst;
using Src = rabbit::Src;
using Core = rabbit::Core;

class MockedMethods
{
   public:
    MOCK_METHOD(void, handleError, (buffer_error));
    MOCK_METHOD(void, handleError, (writer_error));

    template <typename E>
    void expectError(E aError)
    {
        static_assert(
            std::is_enum_v<E>,
            "E must be enum type which values represent error codes.");
        EXPECT_CALL(*this, handleError(aError));
    }
};

template <typename DataT, std::size_t BufSize>
class BinWriterTest : public ::testing::Test
{
   public:
    using Buf = buffer::buffer_view<DataT>;
    using BufConst = buffer::buffer_view_const<DataT>;

    template <typename E>
    void expectError(E aError)
    {
        mDetails->expectError(aError);
    }

    template <typename CallableT>
    void execute(CallableT &&aArg) noexcept
    {
        leaf::try_handle_all(
            aArg,
            [](leaf::match<buffer_error, buffer_error::null_data_and_zero_size>)
            { mDetails->handleError(buffer_error::null_data_and_zero_size); },
            [](leaf::match<buffer_error, buffer_error::null_data>)
            { mDetails->handleError(buffer_error::null_data); },
            [](leaf::match<buffer_error, buffer_error::zero_size>)
            { mDetails->handleError(buffer_error::zero_size); },
            [](leaf::match<buffer_error, buffer_error::invalid_index>)
            { mDetails->handleError(buffer_error::invalid_index); },
            [](leaf::match<writer_error, writer_error::invalid_start_pos>)
            { mDetails->handleError(writer_error::invalid_start_pos); },
            [](leaf::match<writer_error, writer_error::not_enough_buffer_size>)
            { mDetails->handleError(writer_error::not_enough_buffer_size); },
            [](leaf::match<writer_error,
                           writer_error::num_bits_exceed_type_size>)
            { mDetails->handleError(writer_error::num_bits_exceed_type_size); },
            [](leaf::match<writer_error,
                           writer_error::write_more_than_source_size>) {
                mDetails->handleError(
                    writer_error::write_more_than_source_size);
            },
            [](leaf::error_info const &unmatched)
            {
                std::cerr << "Unknown failure detected" << std::endl
                          << "Cryptic diagnostic information follows"
                          << std::endl
                          << unmatched;
            });
    }

    static std::shared_ptr<MockedMethods> mDetails;

   protected:
    static void SetUpTestSuite()
    {
        mDetails = std::make_unique<MockedMethods>();
    }
    static void TearDownTestSuite() { mDetails = nullptr; }

    static inline constexpr std::size_t kSize{BufSize};
    DataT rawBuf_[BufSize]{};
};

template <typename DataT, std::size_t BufSize>
std::shared_ptr<MockedMethods> BinWriterTest<DataT, BufSize>::mDetails;

using BinWriter = BinWriterTest<uint8_t, 64>;

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
            uint8_t arr[] = {0b00000101, 0b00011100, 0b10101010, 0b11001100};
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