#ifndef copy_bits_constexpr_tests_h
#define copy_bits_constexpr_tests_h
#include <gtest/gtest.h>
#include <utils/utils.h>

#include <string>
#include <string_view>
#include <utility>

#include "rabbit/bin_ops.h"
#include "rabbit/details.h"
#include "test_helpers.h"

namespace
{
using namespace std::string_view_literals;

template <typename T>
class CopyBitsCompileTime;

template <auto BitsCount, auto DOffset, auto SOffset, typename CoreT,
          typename DstBitStrT, typename SrcBitStrT>
class CopyBitsCompileTime<
    utils::type_list<utils::value_list<BitsCount, DOffset, SOffset>, CoreT,
                     DstBitStrT, SrcBitStrT>> : public ::testing::Test
{
    using SrcOffset = ::rabbit::SrcOffset;
    using DstOffset = ::rabbit::DstOffset;
    using NumBits = ::rabbit::NumBits;
    using bit_helpers = ::test_utils::bits<CoreT>;
    using Dst = ::rabbit::Dst;
    using Src = ::rabbit::Src;

   protected:
    static inline constexpr NumBits kNBits{BitsCount};
    static inline constexpr DstOffset kDstOffset{DOffset};
    static inline constexpr SrcOffset kSrcOffset{SOffset};

    static constexpr decltype(auto) expected() noexcept
    {
        constexpr auto kDstBitStr = DstBitStrT::kBitsStr;
        constexpr auto kSrcBitStr = SrcBitStrT::kBitsStr;
        auto dstArr = utils::make_array<kDstBitStr.size(), char>(kDstBitStr);
        bit_helpers::copy_bits_expected_str(dstArr.data(), dstArr.size(),
                                            kDstOffset, kSrcBitStr, kSrcOffset,
                                            kNBits);
        return dstArr;
    }

    static constexpr decltype(auto) actual() noexcept
    {
        constexpr auto kDstBitStr = DstBitStrT::kBitsStr;
        auto dstArr =
            bit_helpers::template to_byte_array<kDstBitStr.size()>(kDstBitStr);
        CoreT::copy(Dst(dstArr.data()), kDstOffset,
                    Src(SrcBitStrT::kByteArray.data()), kSrcOffset, kNBits);
        return bit_helpers::to_symbol_array(dstArr);
    }
};

template <auto BitsCount, auto CommonOffset, typename CoreT,
          typename DstBitStrT, typename SrcBitStrT>
class CopyBitsCompileTime<utils::type_list<
    utils::value_list<BitsCount, CommonOffset>, CoreT, DstBitStrT, SrcBitStrT>>
    : public ::testing::Test
{
    using Offset = ::rabbit::Offset;
    using NumBits = ::rabbit::NumBits;
    using bit_helpers = ::test_utils::bits<CoreT>;
    using Dst = ::rabbit::Dst;
    using Src = ::rabbit::Src;

   protected:
    static inline constexpr NumBits kNBits{BitsCount};
    static inline constexpr Offset kOffset{CommonOffset};

    static constexpr decltype(auto) expected() noexcept
    {
        using SrcOffset = ::rabbit::SrcOffset;
        using DstOffset = ::rabbit::DstOffset;
        constexpr auto kDstBitStr = DstBitStrT::kBitsStr;
        constexpr auto kSrcBitStr = SrcBitStrT::kBitsStr;
        auto dstArr = utils::make_array<kDstBitStr.size(), char>(kDstBitStr);
        bit_helpers::copy_bits_expected_str(dstArr.data(), dstArr.size(),
                                            DstOffset{kOffset}, kSrcBitStr,
                                            SrcOffset{kOffset}, kNBits);
        return dstArr;
    }

    static constexpr decltype(auto) actual() noexcept
    {
        constexpr auto kDstBitStr = DstBitStrT::kBitsStr;
        auto dstArr =
            bit_helpers::template to_byte_array<kDstBitStr.size()>(kDstBitStr);
        CoreT::copy(Dst(dstArr.data()), Src(SrcBitStrT::kByteArray.data()),
                    kOffset, kNBits);
        return bit_helpers::to_symbol_array(dstArr);
    }
};

template <auto BitsCount, typename CoreT, typename DstBitStrT,
          typename SrcBitStrT>
class CopyBitsCompileTime<utils::type_list<utils::value_list<BitsCount>, CoreT,
                                           DstBitStrT, SrcBitStrT>>
    : public ::testing::Test
{
    using NumBits = ::rabbit::NumBits;
    using bit_helpers = ::test_utils::bits<CoreT>;
    using Dst = ::rabbit::Dst;
    using Src = ::rabbit::Src;

   protected:
    static inline constexpr NumBits kNBits{BitsCount};

    static constexpr decltype(auto) expected() noexcept
    {
        using SrcOffset = ::rabbit::SrcOffset;
        using DstOffset = ::rabbit::DstOffset;
        constexpr auto kDstBitStr = DstBitStrT::kBitsStr;
        constexpr auto kSrcBitStr = SrcBitStrT::kBitsStr;
        auto dstArr = utils::make_array<kDstBitStr.size(), char>(kDstBitStr);
        bit_helpers::copy_bits_expected_str(dstArr.data(), dstArr.size(),
                                            DstOffset{0}, kSrcBitStr,
                                            SrcOffset{0}, kNBits);
        return dstArr;
    }

    static constexpr decltype(auto) actual() noexcept
    {
        constexpr auto kDstBitStr = DstBitStrT::kBitsStr;
        auto dstArr =
            bit_helpers::template to_byte_array<kDstBitStr.size()>(kDstBitStr);
        CoreT::copy(Dst(dstArr.data()), Src(SrcBitStrT::kByteArray.data()),
                    kNBits);
        return bit_helpers::to_symbol_array(dstArr);
    }
};

// Data to test
template <typename TypeList>
struct testing_type;

template <typename... Ts>
struct testing_type<utils::type_list<Ts...>>
{
    using type = ::testing::Types<Ts...>;
};

template <typename TypeList>
using testing_type_t = typename testing_type<TypeList>::type;

template <typename TypeList, typename... ToMixIn>
struct mixed_in_types;

template <typename... Ts, typename... ToMixIn>
struct mixed_in_types<utils::type_list<Ts...>, ToMixIn...>
{
    using type = utils::type_list<utils::type_list<Ts, ToMixIn...>...>;
};

template <typename TypeList, typename... ToMixIn>
using mixed_in_types_t = typename mixed_in_types<TypeList, ToMixIn...>::type;

using dst_offset_t = rabbit::DstOffset::value_type;
using src_offset_t = rabbit::SrcOffset::value_type;

template <typename CoreT, typename DstT, typename SrcT,
          typename... ValueListsToMix>
struct copy_bits_types
{
    using Combinations =
        typename utils::cartesian_product<ValueListsToMix...>::type;
    using type =
        testing_type_t<mixed_in_types_t<Combinations, CoreT, DstT, SrcT>>;
};

template <typename CoreT, typename DstT, typename SrcT,
          typename... ValueListsToMix>
using copy_bits_types_t =
    typename copy_bits_types<CoreT, DstT, SrcT, ValueListsToMix...>::type;

class ConstexprDstBitStr
{
   public:
    static inline constexpr std::string_view kBitsStr =
        "0011011100100111101001100110010011010010000110101011001001101101000110"
        "0000000111010001110111111011000010010100100111111001011100111000011010"
        "1101011011110101101101101011101010011100111010110000000001011000100001"
        "0111110000110100100101111111100011000111000010"sv;
};

template <typename T>
class ConstexprSrcBitStr
{
   public:
    static inline constexpr std::string_view kBitsStr =
        "1001100111011010001010110010000101001100101100110111110001010100111101"
        "0101011110111010101001010011001100000111101001000011111101001100101100"
        "1010101010111010100110011100100110101110110000011100100000111011100111"
        "0101110111011111110000001001001010100111100101"sv;

    using bit_helpers = ::test_utils::bits<T>;
    static inline constexpr auto kByteArray =
        bit_helpers::template to_byte_array<kBitsStr.size()>(kBitsStr);
};

using DstOffsetsT =
    utils::values_in_range_t<static_cast<dst_offset_t>(0),
                             static_cast<dst_offset_t>(CHAR_BIT - 1)>;
using SrcOffsetsT =
    utils::values_in_range_t<static_cast<src_offset_t>(0),
                             static_cast<src_offset_t>(CHAR_BIT - 1)>;

using NumBits1T = utils::value_list<1_uz, 8_uz, 43_uz, 64_uz, 70_uz>;
using NumBits2T = utils::value_list<127_uz, 128_uz, 129_uz, 230_uz, 248_uz>;

template <typename CoreT, typename NBitsT>
using CopyWith5ArgsT =
    copy_bits_types_t<CoreT, ConstexprDstBitStr, ConstexprSrcBitStr<CoreT>,
                      NBitsT, DstOffsetsT, SrcOffsetsT>;

using offset_t = rabbit::Offset::value_type;
using OffsetsT = utils::values_in_range_t<static_cast<offset_t>(0),
                                          static_cast<offset_t>(CHAR_BIT - 1)>;

template <typename CoreT, typename NBitsT>
using CopyWith4ArgsT =
    copy_bits_types_t<CoreT, ConstexprDstBitStr, ConstexprSrcBitStr<CoreT>,
                      NBitsT, OffsetsT>;

template <typename CoreT, typename NBitsT>
using CopyWith3ArgsT = copy_bits_types_t<CoreT, ConstexprDstBitStr,
                                         ConstexprSrcBitStr<CoreT>, NBitsT>;

TYPED_TEST_SUITE_P(CopyBitsCompileTime);

TYPED_TEST_P(CopyBitsCompileTime, WithArgs)
{
    static constexpr auto kActualArr = TestFixture::actual();
    constexpr std::string_view kActual(kActualArr.data(), kActualArr.size());
    static constexpr auto kExpectedArr = TestFixture::expected();
    constexpr std::string_view kExpected(kExpectedArr.data(),
                                         kExpectedArr.size());
    static_assert(kActual == kExpected);
}

REGISTER_TYPED_TEST_SUITE_P(CopyBitsCompileTime, WithArgs);
}  // namespace

#endif /* copy_bits_constexpr_tests_h */
