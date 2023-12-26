#ifndef rabbit_serialization_tests_h
#define rabbit_serialization_tests_h

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <rabbit/rabbit.h>

template <class T>
using result = rabbit::result<T>;

namespace leaf = boost::leaf;
using buffer_error = buffer::error;
using reader_error = rabbit::reader_error;
using writer_error = rabbit::writer_error;
using Pos = buffer::bit_pos;
using n_bytes = buffer::n_bytes;
using NumBits = rabbit::NumBits;
using bit_pos = buffer::bit_pos;
using Dst = rabbit::Dst;
using Src = rabbit::Src;
using Core = rabbit::Core;
using reader = rabbit::bin_reader<Core>;
using writer = rabbit::bin_writer<Core>;
using DstOffset = rabbit::DstOffset;
using SrcOffset = rabbit::SrcOffset;
using Offset = rabbit::Offset;

class MockedMethods
{
   public:
    MOCK_METHOD(void, handleError, (buffer_error));
    MOCK_METHOD(void, handleError, (reader_error));
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
class Data : public ::testing::Test
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
            [](leaf::match<reader_error, reader_error::invalid_start_pos>)
            { mDetails->handleError(reader_error::invalid_start_pos); },
            [](leaf::match<reader_error, reader_error::not_enough_buffer_size>)
            { mDetails->handleError(reader_error::not_enough_buffer_size); },
            [](leaf::match<reader_error,
                           reader_error::num_bits_exceed_type_size>)
            { mDetails->handleError(reader_error::num_bits_exceed_type_size); },
            [](leaf::match<reader_error,
                           reader_error::read_more_than_destination_size>) {
                mDetails->handleError(
                    reader_error::read_more_than_destination_size);
            },
            [](leaf::match<reader_error, reader_error::invalid_interval_index>)
            { mDetails->handleError(reader_error::invalid_interval_index); },
            [](leaf::match<reader_error,
                           reader_error::non_empty_vector_size_is_zero>) {
                mDetails->handleError(
                    reader_error::non_empty_vector_size_is_zero);
            },
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
            [](leaf::match<writer_error, writer_error::value_below_interval>)
            { mDetails->handleError(writer_error::value_below_interval); },
            [](leaf::match<writer_error, writer_error::value_above_interval>)
            { mDetails->handleError(writer_error::value_above_interval); },
            [](leaf::match<writer_error, writer_error::vector_size_is_too_big>)
            { mDetails->handleError(writer_error::vector_size_is_too_big); },
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
std::shared_ptr<MockedMethods> Data<DataT, BufSize>::mDetails;

struct Empty
{
};

struct NotEmpty
{
    std::uint16_t a;
    std::uint16_t b;
};

struct Nested
{
    std::uint16_t a;
    NotEmpty b;
    std::uint16_t c;
};

struct Unsupported
{
    std::uint16_t a;
    float u;
    std::uint8_t b;
};

struct NestedWithUnsupported
{
    std::uint16_t a;
    NotEmpty b;
    std::uint16_t c;
    Unsupported d;
};

#endif /* rabbit_serialization_tests_h */
