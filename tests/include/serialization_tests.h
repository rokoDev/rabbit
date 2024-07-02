#ifndef rabbit_serialization_tests_h
#define rabbit_serialization_tests_h

#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <rabbit/rabbit.h>

using buffer_error = buffer::error;
using reader_error = rabbit::reader_error;
using writer_error = rabbit::writer_error;
using Pos = buffer::bit_pos;
using n_bytes = buffer::n_bytes;
using NumBits = rabbit::NumBits;
using bit_pos = buffer::bit_pos;
using Dst = rabbit::Dst;
using Src = rabbit::Src;
using core = rabbit::core;
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
    void execute(CallableT &&) noexcept
    {
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
