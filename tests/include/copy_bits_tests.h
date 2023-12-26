#ifndef copy_bits_tests_h
#define copy_bits_tests_h

#include <gtest/gtest.h>

#include <string_view>
#include <tuple>

#include "rabbit/bin_ops.h"
#include "test_helpers.h"

namespace test
{
using ::testing::TestWithParam;

using namespace std::string_view_literals;
using DstOffset = ::rabbit::DstOffset;
using SrcOffset = ::rabbit::SrcOffset;
using NumBits = ::rabbit::NumBits;
using Offset = ::rabbit::Offset;
using Src = ::rabbit::Src;
using Dst = ::rabbit::Dst;

template <std::size_t ByteCount>
struct bits_data_t
{
    std::string_view str;
    std::array<std::byte, ByteCount> bytes;
};

template <std::size_t ByteCount>
bits_data_t(const std::string_view &, const std::array<std::byte, ByteCount> &)
    -> bits_data_t<ByteCount>;

struct src_data_t
{
    std::string_view str;
    Src bytes;
    std::size_t index;
};

using src_bits_t = src_data_t;
using dst_bits_t = src_data_t;

using CopyBits5TestDataT =
    std::tuple<dst_bits_t, src_bits_t, NumBits, DstOffset, SrcOffset>;
using CopyBits4TestDataT = std::tuple<dst_bits_t, src_bits_t, NumBits, Offset>;
using CopyBits3TestDataT = std::tuple<dst_bits_t, src_bits_t, NumBits>;

template <typename CoreT>
class CopyBitsBase
{
   protected:
    using core = CoreT;
    using test_utils = ::test_utils::bits<CoreT>;

    std::vector<std::byte> to_buf(dst_bits_t &kDstData)
    {
        using rabbit::details::bytes_count;
        std::vector<std::byte> actual_dst_buf(
            kDstData.bytes.get(),
            (kDstData.bytes + bytes_count(NumBits{kDstData.str.size()})).get());
        return actual_dst_buf;
    }

    std::vector<char> to_bits_str(const std::vector<std::byte> &aBuf)
    {
        std::vector<char> actual_dst_bits(aBuf.size() * CHAR_BIT);
        test_utils::to_symbol_buf(actual_dst_bits.data(), Src{aBuf.data()},
                                  actual_dst_bits.size());
        return actual_dst_bits;
    }
};

template <typename CoreT>
class CopyBitsWith5Args
    : public CopyBitsBase<CoreT>
    , public TestWithParam<CopyBits5TestDataT>
{
};

template <typename CoreT>
class CopyBitsWith4Args
    : public CopyBitsBase<CoreT>
    , public TestWithParam<CopyBits4TestDataT>
{
};

template <typename CoreT>
class CopyBitsWith3Args
    : public CopyBitsBase<CoreT>
    , public TestWithParam<CopyBits3TestDataT>
{
};

inline constexpr auto kDstOffsetsArray =
    utils::make_array_from_range<0_uf8, 7_uf8, 1_uf8, DstOffset>();
inline constexpr auto kSrcOffsetsArray =
    utils::make_array_from_range<0_uf8, 7_uf8, 1_uf8, SrcOffset>();
inline constexpr auto kBitOffsetsArray =
    utils::make_array_from_range<0_uf8, 7_uf8, 1_uf8, Offset>();
inline constexpr auto kNBitsArray = utils::concatenate_arrays(
    utils::make_array_from_range<0_uz, 170_uz, 1_uz, NumBits>(),
    utils::make_array_from_range<230_uz, 248_uz, 1_uz, NumBits>());

namespace details
{
inline constexpr auto kDstBits = utils::make_array(
    "0100011000101100101011110101001100000011011011110100111010011001010001001100100110111011110101001110011101110010101011011101011011110000000011111111100100001110100100110110011110101101000111100010111111011100110011111001111011110110100000100100111101111010"sv);

inline constexpr auto kSrcBits = utils::make_array(
    "1101011011100000001100001110110010100101000111000001000101100111111111000011010110000101000111011110011101110010011111110010110100010010101010000101101100100111101111001010101110110011111010100111010110001101010110001101010010100100100011100110111000110001"sv);

template <typename CoreT, auto &BitsStrArrRef, std::size_t... I>
constexpr decltype(auto) make_bits_data_tuple(
    std::index_sequence<I...>) noexcept
{
    using test_utils = ::test_utils::bits<CoreT>;
    return std::make_tuple(
        bits_data_t{BitsStrArrRef[I],
                    test_utils::template to_byte_array<BitsStrArrRef[I].size()>(
                        BitsStrArrRef[I])}...);
}

template <typename CoreT>
inline constexpr auto kDstBitDatas = make_bits_data_tuple<CoreT, kDstBits>(
    std::make_index_sequence<kDstBits.size()>{});

template <typename CoreT>
inline constexpr auto kSrcBitDatas = make_bits_data_tuple<CoreT, kSrcBits>(
    std::make_index_sequence<kDstBits.size()>{});

template <std::size_t... ByteCounts, std::size_t... I>
constexpr decltype(auto) make_bits_data_arr(
    const std::tuple<bits_data_t<ByteCounts>...> &aValue,
    std::index_sequence<I...>) noexcept
{
    return utils::make_array(src_data_t{
        std::get<I>(aValue).str, Src{std::get<I>(aValue).bytes.data()}, I}...);
}
}  // namespace details

#ifdef CORE_V1
using core_t = ::rabbit::v1::Core;
#endif

#ifdef CORE_V2
using core_t = ::rabbit::v2::Core;
#endif
using details::kDstBitDatas;
using details::kSrcBitDatas;
using details::make_bits_data_arr;
using DstIndices =
    std::make_index_sequence<std::tuple_size_v<decltype(kDstBitDatas<core_t>)>>;
using SrcIndices =
    std::make_index_sequence<std::tuple_size_v<decltype(kSrcBitDatas<core_t>)>>;
inline constexpr auto kDstData =
    make_bits_data_arr(kDstBitDatas<core_t>, DstIndices{});
inline constexpr auto kSrcData =
    make_bits_data_arr(kSrcBitDatas<core_t>, SrcIndices{});

using CopyBits5Args = CopyBitsWith5Args<core_t>;
using CopyBits4Args = CopyBitsWith4Args<core_t>;
using CopyBits3Args = CopyBitsWith3Args<core_t>;
}  // namespace test

#endif /* copy_bits_tests_h */
