#ifndef copy_bits_constexpr_tests_h
#define copy_bits_constexpr_tests_h
#include <gtest/gtest.h>

#include <string>
#include <string_view>
#include <utility>

#include "rabbit/bin_ops.h"
#include "rabbit/details.h"
#include "test_helpers.h"

namespace rabbit
{
namespace test
{
using namespace std::string_view_literals;

template <typename T>
class CopyBitsCompileTime : public ::testing::Test
{
    using SrcBitOffset = rabbit::SrcBitOffset;
    using DstBitOffset = rabbit::DstBitOffset;
    using NumBits = rabbit::NumBits;
    using helpers = rabbit::test_helpers;

    static inline constexpr std::string_view kSrc256BitsStr =
        "1001100111011010001010110010000101001100101100110111110001010100111101"
        "0101011110111010101001010011001100000111101001000011111101001100101100"
        "1010101010111010100110011100100110101110110000011100100000111011100111"
        "0101110111011111110000001001001010100111100101"sv;

    static inline constexpr std::string_view kDst256BitsStr =
        "0011011100100111101001100110010011010010000110101011001001101101000110"
        "0000000111010001110111111011000010010100100111111001011100111000011010"
        "1101011011110101101101101011101010011100111010110000000001011000100001"
        "0111110000110100100101111111100011000111000010"sv;

    static inline constexpr auto kSrc256BitsArray =
        helpers::to_uint8_array<kSrc256BitsStr.size()>(kSrc256BitsStr);

   protected:
    static inline constexpr NumBits NBits{T::template get<0>()};
    static inline constexpr DstBitOffset DstOffset{T::template get<1>()};
    static inline constexpr SrcBitOffset SrcOffset{T::template get<2>()};

    static constexpr decltype(auto) expected(
        std::string_view aDstData = kDst256BitsStr,
        std::string_view aSrcData = kSrc256BitsStr) noexcept
    {
        constexpr std::size_t kDstNBytes =
            rabbit::details::bytesCount(DstOffset.get(), NBits.get());
        auto dstBuf = std::array<uint8_t, kDstNBytes>{};
        auto dstBits = aDstData.substr(0, kDstNBytes * CHAR_BIT);

        helpers::copyBitsExpected(dstBuf.data(), dstBits, DstOffset, aSrcData,
                                  SrcOffset, NBits);

        return dstBuf;
    }

    static constexpr decltype(auto) actual(
        std::string_view aDstData = kDst256BitsStr,
        uint8_t const* const aSrcBuf = kSrc256BitsArray.data()) noexcept
    {
        constexpr std::size_t kDstNBytes =
            rabbit::details::bytesCount(DstOffset.get(), NBits.get());
        auto dstBuf = std::array<uint8_t, kDstNBytes>{};
        auto dstBits = aDstData.substr(0, kDstNBytes * CHAR_BIT);

        helpers::to_uint8_buf(dstBuf.data(), dstBits);

        rabbit::BinOps::copyBits(dstBuf.data(), DstOffset, aSrcBuf, SrcOffset,
                                 NBits);

        return dstBuf;
    }
};

TYPED_TEST_SUITE_P(CopyBitsCompileTime);

TYPED_TEST_P(CopyBitsCompileTime, With5ArgsNBitsDstOffsetSrcOffset)
{
    constexpr auto kActual = TestFixture::actual();
    constexpr auto kExpected = TestFixture::expected();
    constexpr bool kSuccess = rabbit::is_equal(kActual, kExpected);
    static_assert(kSuccess, "Unexpected kResult.");
}

REGISTER_TYPED_TEST_SUITE_P(CopyBitsCompileTime,
                            With5ArgsNBitsDstOffsetSrcOffset);

template <typename TypeList>
struct testing_type;

template <typename... Ts>
struct testing_type<rabbit::type_list<Ts...>>
{
    using type = ::testing::Types<Ts...>;
};

template <typename TypeList>
using testing_type_t = typename testing_type<TypeList>::type;

using dst_offset_t = rabbit::DstBitOffset::value_type;
using src_offset_t = rabbit::DstBitOffset::value_type;

using DstOffsetsT =
    rabbit::values_in_range_t<static_cast<dst_offset_t>(0),
                              static_cast<dst_offset_t>(CHAR_BIT - 1)>;
using SrcOffsetsT =
    rabbit::values_in_range_t<static_cast<src_offset_t>(0),
                              static_cast<src_offset_t>(CHAR_BIT - 1)>;

template <auto... NBitVals>
struct copy_bits_types
{
    using NumBits = rabbit::values<NBitVals...>;
    using Combinations =
        rabbit::cartesian_product_t<NumBits, DstOffsetsT, SrcOffsetsT>;
    using type = testing_type_t<Combinations>;
};

template <auto... NBitVals>
using copy_bits_types_t = typename copy_bits_types<NBitVals...>::type;
}  // namespace test
}  // namespace rabbit

#endif /* copy_bits_constexpr_tests_h */
