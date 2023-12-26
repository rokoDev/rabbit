#include <gtest/gtest.h>
#include <utils/utils.h>

#include <array>
#include <string_view>
#include <type_traits>

#include "test_helpers.h"

namespace
{
using namespace std::string_view_literals;

template <typename Mode>
using bit_helpers = ::test_utils::bits<Mode>;

using ::testing::Test;
using SrcOffset = rabbit::SrcOffset;
using DstOffset = rabbit::DstOffset;
using NumBits = rabbit::NumBits;

class GetValueExpected : public Test
{
   protected:
    static inline constexpr auto k72SrcBitStr =
        "110011101101000101011001000010100110010110011011111000101010011111110101"sv;
    static constexpr auto kNBits = k72SrcBitStr.size();
};

template <typename CoreT, typename T, T Value>
class AddValueExpected : public GetValueExpected
{
   protected:
    using helpers = bit_helpers<CoreT>;
    using uint = T;
    using DataT = std::tuple<std::string_view, uint, DstOffset, NumBits>;
    static constexpr uint kValue = Value;
};

template <typename CoreT>
using AddValueExpected8 = AddValueExpected<CoreT, std::uint8_t, 0b10110011>;

template <typename CoreT>
using AddValueExpected16 =
    AddValueExpected<CoreT, std::uint16_t, 0b10111001'00111101>;

template <typename CoreT>
using AddValueExpected32 =
    AddValueExpected<CoreT, std::uint32_t,
                     0b10011010'01000011'01010110'01001101>;

template <typename CoreT>
using AddValueExpected64 = AddValueExpected<
    CoreT, std::uint64_t,
    0b11100100'11110100'11001100'10011010'01000011'01010110'01001101'10100011>;

using AddValueExpected8V1 = AddValueExpected8<::rabbit::v1::Core>;
using AddValueExpected16V1 = AddValueExpected16<::rabbit::v1::Core>;
using AddValueExpected32V1 = AddValueExpected32<::rabbit::v1::Core>;
using AddValueExpected64V1 = AddValueExpected64<::rabbit::v1::Core>;

using AddValueExpected8V2 = AddValueExpected8<::rabbit::v2::Core>;
using AddValueExpected16V2 = AddValueExpected16<::rabbit::v2::Core>;
using AddValueExpected32V2 = AddValueExpected32<::rabbit::v2::Core>;
using AddValueExpected64V2 = AddValueExpected64<::rabbit::v2::Core>;

template <typename T>
class CopyExpected : public Test
{
   protected:
    static inline constexpr std::string_view kSrcBitsStr =
        "1001100111011010001010110010000101001100101100110111110001010100111101"sv;

    static inline constexpr std::string_view kDstBitsStr =
        "0011011100100111101001100110010011010010000110101011001001101101000110"sv;

    using helpers = bit_helpers<T>;

    std::array<std::string_view::value_type, kDstBitsStr.size()> dstArr =
        utils::make_array<kDstBitsStr.size(), char>(kDstBitsStr);
    std::string_view dst{dstArr.data(), dstArr.size()};
    constexpr void expected(DstOffset aDstOffset, SrcOffset aSrcOffset,
                            NumBits aNBits) noexcept
    {
        helpers::copy_bits_expected_str(dstArr.data(), dstArr.size(),
                                        aDstOffset, kSrcBitsStr, aSrcOffset,
                                        aNBits);
    }
};

using CopyExpectedV1 = CopyExpected<::rabbit::v1::Core>;
using CopyExpectedV2 = CopyExpected<::rabbit::v2::Core>;

TEST_F(CopyExpectedV1, Dst0Src0NBits0True)
{
    constexpr std::string_view kValidExpected =
        "0011011100100111101001100110010011010010000110101011001001101101000110"sv;
    expected(DstOffset{0}, SrcOffset{0}, NumBits{0});
    ASSERT_EQ(dst, kValidExpected);
}

TEST_F(CopyExpectedV2, Dst0Src0NBits0False)
{
    constexpr std::string_view kValidExpected =
        "0011011100100111101001100110010011010010000110101011001001101101000110"sv;
    expected(DstOffset{0}, SrcOffset{0}, NumBits{0});
    ASSERT_EQ(dst, kValidExpected);
}

TEST_F(CopyExpectedV1, Dst0Src0NBits1True)
{
    constexpr std::string_view kValidExpected =
        "1011011100100111101001100110010011010010000110101011001001101101000110"sv;
    expected(DstOffset{0}, SrcOffset{0}, NumBits{1});
    ASSERT_EQ(dst, kValidExpected);
}

TEST_F(CopyExpectedV2, Dst0Src0NBits1False)
{
    constexpr std::string_view kValidExpected =
        "0011011100100111101001100110010011010010000110101011001001101101000111"sv;
    expected(DstOffset{0}, SrcOffset{0}, NumBits{1});
    ASSERT_EQ(dst, kValidExpected);
}

TEST_F(CopyExpectedV1, Dst0Src0NBits10True)
{
    constexpr std::string_view kValidExpected =
        "1001100111100111101001100110010011010010000110101011001001101101000110"sv;
    expected(DstOffset{0}, SrcOffset{0}, NumBits{10});
    ASSERT_EQ(dst, kValidExpected);
}

TEST_F(CopyExpectedV2, Dst0Src0NBits10False)
{
    constexpr std::string_view kValidExpected =
        "0011011100100111101001100110010011010010000110101011001001100100111101"sv;
    expected(DstOffset{0}, SrcOffset{0}, NumBits{10});
    ASSERT_EQ(dst, kValidExpected);
}

TEST_F(CopyExpectedV1, Dst0Src3NBits10True)
{
    constexpr std::string_view kValidExpected =
        "1100111011100111101001100110010011010010000110101011001001101101000110"sv;
    expected(DstOffset{0}, SrcOffset{3}, NumBits{10});
    ASSERT_EQ(dst, kValidExpected);
}

TEST_F(CopyExpectedV2, Dst0Src3NBits10False)
{
    constexpr std::string_view kValidExpected =
        "0011011100100111101001100110010011010010000110101011001001101010100111"sv;
    expected(DstOffset{0}, SrcOffset{3}, NumBits{10});
    ASSERT_EQ(dst, kValidExpected);
}

TEST_F(CopyExpectedV1, Dst5Src3NBits10True)
{
    constexpr std::string_view kValidExpected =
        "0011011001110111101001100110010011010010000110101011001001101101000110"sv;
    expected(DstOffset{5}, SrcOffset{3}, NumBits{10});
    ASSERT_EQ(dst, kValidExpected);
}

TEST_F(CopyExpectedV2, Dst5Src3NBits10False)
{
    constexpr std::string_view kValidExpected =
        "0011011100100111101001100110010011010010000110101011001101010011100110"sv;
    expected(DstOffset{5}, SrcOffset{3}, NumBits{10});
    ASSERT_EQ(dst, kValidExpected);
}

TEST_F(CopyExpectedV1, Dst0Src0NBits70True)
{
    constexpr std::string_view kValidExpected =
        "1001100111011010001010110010000101001100101100110111110001010100111101"sv;
    expected(DstOffset{0}, SrcOffset{0}, NumBits{70});
    ASSERT_EQ(dst, kValidExpected);
}

TEST_F(CopyExpectedV2, Dst0Src0NBits70False)
{
    constexpr std::string_view kValidExpected =
        "1001100111011010001010110010000101001100101100110111110001010100111101"sv;
    expected(DstOffset{0}, SrcOffset{0}, NumBits{70});
    ASSERT_EQ(dst, kValidExpected);
}

TEST_F(CopyExpectedV1, ToBinArray1A1)
{
    constexpr auto kBitStr = ""sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 0> kExpectedArray{};
    static_assert(utils::is_equal(kResultArray, kExpectedArray));
}

TEST_F(CopyExpectedV2, ToBinArray1A2)
{
    constexpr auto kBitStr = ""sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 0> kExpectedArray{};
    static_assert(utils::is_equal(kResultArray, kExpectedArray));
}

TEST_F(CopyExpectedV1, ToBinArray2A1)
{
    constexpr auto kBitStr = "1"sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 1> kExpectedArray{0b10000000_b};
    static_assert(utils::is_equal(kResultArray, kExpectedArray));
}

TEST_F(CopyExpectedV2, ToBinArray2A2)
{
    constexpr auto kBitStr = "1"sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 1> kExpectedArray{0b00000001_b};
    static_assert(utils::is_equal(kResultArray, kExpectedArray));
}

TEST_F(CopyExpectedV1, ToBinArray3A1)
{
    constexpr auto kBitStr = "0"sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 1> kExpectedArray{0b00000000_b};
    static_assert(utils::is_equal(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST_F(CopyExpectedV2, ToBinArray3A2)
{
    constexpr auto kBitStr = "0"sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 1> kExpectedArray{0b00000000_b};
    static_assert(utils::is_equal(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST_F(CopyExpectedV1, ToBinArray4A1)
{
    constexpr auto kBitStr = "101"sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 1> kExpectedArray{0b10100000_b};
    static_assert(utils::is_equal(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST_F(CopyExpectedV2, ToBinArray4A2)
{
    constexpr auto kBitStr = "101"sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 1> kExpectedArray{0b00000101_b};
    static_assert(utils::is_equal(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST_F(CopyExpectedV1, ToBinArray5A1)
{
    constexpr auto kBitStr = "10100001"sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 1> kExpectedArray{0b10100001_b};
    static_assert(utils::is_equal(kResultArray, kExpectedArray));
}

TEST_F(CopyExpectedV2, ToBinArray5A2)
{
    constexpr auto kBitStr = "10100001"sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 1> kExpectedArray{0b10100001_b};
    ASSERT_TRUE(utils::is_equal(kResultArray, kExpectedArray));
    static_assert(utils::is_equal(kResultArray, kExpectedArray));
}

TEST_F(CopyExpectedV1, ToBinArray6A1)
{
    constexpr auto kBitStr = "101001011"sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 2> kExpectedArray{0b10100101_b,
                                                      0b10000000_b};
    static_assert(utils::is_equal(kResultArray, kExpectedArray));
}

TEST_F(CopyExpectedV2, ToBinArray6A2)
{
    constexpr auto kBitStr = "101001011"sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 2> kExpectedArray{0b01001011_b,
                                                      0b00000001_b};
    static_assert(utils::is_equal(kResultArray, kExpectedArray));
}

TEST_F(CopyExpectedV1, ToBinArray7A1)
{
    constexpr auto kBitStr = "10101010011111100100001"sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 3> kExpectedArray{
        0b10101010_b, 0b01111110_b, 0b01000010_b};
    static_assert(utils::is_equal(kResultArray, kExpectedArray));
}

TEST_F(CopyExpectedV2, ToBinArray7A2)
{
    constexpr auto kBitStr = "10101010011111100100001"sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 3> kExpectedArray{
        0b00100001_b, 0b00111111_b, 0b01010101_b};
    static_assert(utils::is_equal(kResultArray, kExpectedArray));
}

TEST_F(CopyExpectedV1, ToBinArray8A1)
{
    constexpr auto kBitStr = "1010101001111110110000111"sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 4> kExpectedArray{
        0b10101010_b, 0b01111110_b, 0b11000011_b, 0b10000000_b};
    static_assert(utils::is_equal(kResultArray, kExpectedArray));
}

TEST_F(CopyExpectedV2, ToBinArray8A2)
{
    constexpr auto kBitStr = "1010101001111110110000111"sv;
    constexpr auto kResultArray =
        helpers::to_byte_array<kBitStr.size()>(kBitStr);
    constexpr std::array<std::byte, 4> kExpectedArray{
        0b10000111_b, 0b11111101_b, 0b01010100_b, 0b00000001_b};
    static_assert(utils::is_equal(kResultArray, kExpectedArray));
}

TEST_F(CopyExpectedV1, ToBitStringView1A1)
{
    constexpr std::array<std::byte, 4> kBitsArray{0b10101010_b, 0b01111110_b,
                                                  0b11000011_b, 0b10000000_b};
    static constexpr auto kSymbols =
        helpers::to_symbol_array(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "10101010011111101100001110000000";
    static_assert(kResult == kExpected);
}

TEST_F(CopyExpectedV2, ToBitStringView1A2)
{
    constexpr std::array<std::byte, 4> kBitsArray{0b10101010_b, 0b01111110_b,
                                                  0b11000011_b, 0b10000000_b};
    static constexpr auto kSymbols =
        helpers::to_symbol_array(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "10000000110000110111111010101010";
    static_assert(kResult == kExpected);
}

TEST_F(CopyExpectedV1, ToBitStringView2A1)
{
    constexpr std::array<std::byte, 4> kBitsArray{0b10101010_b, 0b01111110_b,
                                                  0b11000011_b, 0b10000000_b};
    static constexpr auto kSymbols =
        helpers::to_symbol_array<25>(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "1010101001111110110000111";
    static_assert(kResult == kExpected);
}

TEST_F(CopyExpectedV2, ToBitStringView2A2)
{
    constexpr std::array<std::byte, 4> kBitsArray{0b10101010_b, 0b01111110_b,
                                                  0b11000011_b, 0b10000000_b};
    static constexpr auto kSymbols =
        helpers::to_symbol_array<25>(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "0110000110111111010101010";
    static_assert(kResult == kExpected);
}

TEST_F(CopyExpectedV1, ToBitStringView3A1)
{
    constexpr std::array<std::byte, 0> kBitsArray{};
    static constexpr auto kSymbols =
        helpers::to_symbol_array(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "";
    static_assert(kResult == kExpected);
}

TEST_F(CopyExpectedV2, ToBitStringView3A2)
{
    constexpr std::array<std::byte, 0> kBitsArray{};
    static constexpr auto kSymbols =
        helpers::to_symbol_array(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "";
    static_assert(kResult == kExpected);
}

TEST_F(CopyExpectedV1, ToBitStringView4A1)
{
    constexpr std::array<std::byte, 1> kBitsArray{0b10000000_b};
    static constexpr auto kSymbols =
        helpers::to_symbol_array(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "10000000";
    static_assert(kResult == kExpected);
}

TEST_F(CopyExpectedV2, ToBitStringView4A2)
{
    constexpr std::array<std::byte, 1> kBitsArray{0b10000000_b};
    static constexpr auto kSymbols =
        helpers::to_symbol_array(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "10000000";
    static_assert(kResult == kExpected);
}

TEST_F(CopyExpectedV1, ToBitStringView5A1)
{
    constexpr std::array<std::byte, 1> kBitsArray{0b10001100_b};
    static constexpr auto kSymbols =
        helpers::to_symbol_array<1>(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "1";
    static_assert(kResult == kExpected);
}

TEST_F(CopyExpectedV2, ToBitStringView5A2)
{
    constexpr std::array<std::byte, 1> kBitsArray{0b10001100_b};
    static constexpr auto kSymbols =
        helpers::to_symbol_array<1>(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "0";
    static_assert(kResult == kExpected);
}

TEST_F(CopyExpectedV1, ToBitStringView6A1)
{
    constexpr std::array<std::byte, 1> kBitsArray{0b10001100_b};
    static constexpr auto kSymbols =
        helpers::to_symbol_array<5>(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "10001";
    static_assert(kResult == kExpected);
}

TEST_F(CopyExpectedV2, ToBitStringView6A2)
{
    constexpr std::array<std::byte, 1> kBitsArray{0b10001100_b};
    static constexpr auto kSymbols =
        helpers::to_symbol_array<5>(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "01100";
    static_assert(kResult == kExpected);
}

TEST_F(AddValueExpected8V1, UInt8kOffset0kNBits0A1)
{
    using U = std::uint8_t;
    const U kExpected{0b00000000};
    const auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, SrcOffset{0}, NumBits{0});
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected8V2, UInt8kOffset0kNBits0A2)
{
    using U = std::uint8_t;
    const U kExpected{0b00000000};
    const auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, SrcOffset{0}, NumBits{0});
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected8V1, UInt8kOffset0kNBits3A1)
{
    using U = std::uint8_t;
    const U kExpected{0b00000110};
    const auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, SrcOffset{0}, NumBits{3});
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected8V2, UInt8kOffset0kNBits3A2)
{
    using U = std::uint8_t;
    const U kExpected{0b00000101};
    const auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, SrcOffset{0}, NumBits{3});
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected8V1, ConstexprUInt8kOffset3kNBits2A1)
{
    using U = std::uint8_t;
    constexpr U kExpected{0b00000001};
    constexpr SrcOffset kOffset{3};
    constexpr NumBits kBits{2};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected8V2, ConstexprUInt8kOffset3kNBits2A2)
{
    using U = std::uint8_t;
    constexpr U kExpected{0b00000010};
    constexpr SrcOffset kOffset{3};
    constexpr NumBits kBits{2};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected8V1, ConstexprUInt8kOffset7kNBits8A1)
{
    using U = std::uint8_t;
    constexpr U kExpected{0b01101000};
    constexpr SrcOffset kOffset{7};
    constexpr NumBits kBits{8};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected8V2, ConstexprUInt8kOffset7kNBits8A2)
{
    using U = std::uint8_t;
    constexpr U kExpected{0b01001111};
    constexpr SrcOffset kOffset{7};
    constexpr NumBits kBits{8};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected32V1, ConstexprUInt32kOffset7kNBits1A1)
{
    using U = std::uint32_t;
    constexpr U kExpected{0b00000000'00000000'00000000'00000000};
    constexpr SrcOffset kOffset{7};
    constexpr NumBits kBits{1};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected32V2, ConstexprUInt32kOffset7kNBits1A2)
{
    using U = std::uint32_t;
    constexpr U kExpected{0b00000000'00000000'00000000'00000001};
    constexpr SrcOffset kOffset{7};
    constexpr NumBits kBits{1};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected32V1, ConstexprUInt32kOffset7kNBits21A1)
{
    using U = std::uint32_t;
    constexpr U kExpected{0b00000000'00001101'00010101'10010000};
    constexpr SrcOffset kOffset{7};
    constexpr NumBits kBits{21};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected32V2, ConstexprUInt32kOffset7kNBits21A2)
{
    using U = std::uint32_t;
    constexpr U kExpected{0b00000000'00010111'11000101'01001111};
    constexpr SrcOffset kOffset{7};
    constexpr NumBits kBits{21};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected32V1, UInt32kOffset7kNBits21A1)
{
    using U = std::uint32_t;
    U kExpected{0b00000000'00001101'00010101'10010000};
    SrcOffset kOffset{7};
    NumBits kBits{21};
    auto kActual = helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected32V2, UInt32kOffset7kNBits21A2)
{
    using U = std::uint32_t;
    U kExpected{0b00000000'00010111'11000101'01001111};
    SrcOffset kOffset{7};
    NumBits kBits{21};
    auto kActual = helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected64V1, UInt64kOffset6kNBits2A1)
{
    using U = std::uint64_t;
    U kExpected{
        0b00000000'00000000'00000000'00000000'00000000'00000000'00000000'00000010};
    SrcOffset kOffset{6};
    NumBits kBits{2};
    auto kActual = helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected64V2, UInt64kOffset6kNBits2A2)
{
    using U = std::uint64_t;
    U kExpected{
        0b00000000'00000000'00000000'00000000'00000000'00000000'00000000'00000011};
    SrcOffset kOffset{6};
    NumBits kBits{2};
    auto kActual = helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected64V1, ConstexprUInt64kOffset6kNBits2A1)
{
    using U = std::uint64_t;
    constexpr U kExpected{
        0b00000000'00000000'00000000'00000000'00000000'00000000'00000000'00000010};
    constexpr SrcOffset kOffset{6};
    constexpr NumBits kBits{2};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected64V2, ConstexprUInt64kOffset6kNBits2A2)
{
    using U = std::uint64_t;
    constexpr U kExpected{
        0b00000000'00000000'00000000'00000000'00000000'00000000'00000000'00000011};
    constexpr SrcOffset kOffset{6};
    constexpr NumBits kBits{2};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected64V1, UInt64kOffset5kNBits64A1)
{
    using U = std::uint64_t;
    U kExpected{
        0b11011010'00101011'00100001'01001100'10110011'01111100'01010100'11111110};
    SrcOffset kOffset{5};
    NumBits kBits{64};
    auto kActual = helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected64V2, UInt64kOffset5kNBits64A2)
{
    using U = std::uint64_t;
    U kExpected{
        0b01110110'10001010'11001000'01010011'00101100'11011111'00010101'00111111};
    SrcOffset kOffset{5};
    NumBits kBits{64};
    auto kActual = helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected64V1, ConstexprUInt64kOffset5kNBits64A1)
{
    using U = std::uint64_t;
    constexpr U kExpected{
        0b11011010'00101011'00100001'01001100'10110011'01111100'01010100'11111110};
    constexpr SrcOffset kOffset{5};
    constexpr NumBits kBits{64};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected64V2, ConstexprUInt64kOffset5kNBits64A2)
{
    using U = std::uint64_t;
    constexpr U kExpected{
        0b01110110'10001010'11001000'01010011'00101100'11011111'00010101'00111111};
    constexpr SrcOffset kOffset{5};
    constexpr NumBits kBits{64};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected8V1, Offset0kNBits0A1)
{
    constexpr auto kExpected = k72SrcBitStr;
    const auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{0}, kValue, NumBits{0});
    const auto kActual = std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected8V2, Offset0kNBits0A2)
{
    constexpr auto kExpected = k72SrcBitStr;
    const auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{0}, kValue, NumBits{0});
    const auto kActual = std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected8V1, Offset0kNBits3A1)
{
    constexpr auto kExpected =
        "011011101101000101011001000010100110010110011011111000101010011111110101"sv;
    const auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{0}, kValue, NumBits{3});
    const auto kActual = std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected8V2, Offset0kNBits3A2)
{
    constexpr auto kExpected =
        "110011101101000101011001000010100110010110011011111000101010011111110011"sv;
    const auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{0}, kValue, NumBits{3});
    const auto kActual = std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected8V1, ConstexprOffset3kNBits2A1)
{
    constexpr auto kExpected =
        "110111101101000101011001000010100110010110011011111000101010011111110101"sv;
    static constexpr auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{3}, kValue, NumBits{2});
    constexpr auto kActual =
        std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected8V2, ConstexprOffset3kNBits2A2)
{
    constexpr auto kExpected =
        "110011101101000101011001000010100110010110011011111000101010011111111101"sv;
    static constexpr auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{3}, kValue, NumBits{2});
    constexpr auto kActual =
        std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected8V1, ConstexprOffset7kNBits8A1)
{
    constexpr auto kExpected =
        "110011110110011101011001000010100110010110011011111000101010011111110101"sv;
    static constexpr auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{7}, kValue, NumBits{8});
    constexpr auto kActual =
        std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected8V2, ConstexprOffset7kNBits8A2)
{
    constexpr auto kExpected =
        "110011101101000101011001000010100110010110011011111000101101100111110101"sv;
    static constexpr auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{7}, kValue, NumBits{8});
    constexpr auto kActual =
        std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected32V1, ConstexprOffset7kNBits1A1)
{
    constexpr auto kExpected =
        "110011111101000101011001000010100110010110011011111000101010011111110101"sv;
    static constexpr auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{7}, kValue, NumBits{1});
    constexpr auto kActual =
        std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected32V2, ConstexprOffset7kNBits1A2)
{
    constexpr auto kExpected = k72SrcBitStr;
    static constexpr auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{7}, kValue, NumBits{1});
    constexpr auto kActual =
        std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected32V1, ConstexprOffset7kNBits21A1)
{
    constexpr auto kExpected =
        "110011100011010101100100110110100110010110011011111000101010011111110101"sv;
    static constexpr auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{7}, kValue, NumBits{21});
    constexpr auto kActual =
        std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected32V2, ConstexprOffset7kNBits21A2)
{
    constexpr auto kExpected =
        "110011101101000101011001000010100110010110010001101010110010011011110101"sv;
    static constexpr auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{7}, kValue, NumBits{21});
    constexpr auto kActual =
        std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected32V1, Offset7kNBits21A1)
{
    constexpr auto kExpected =
        "110011100011010101100100110110100110010110011011111000101010011111110101"sv;
    const auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{7}, kValue, NumBits{21});
    const auto kActual = std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected32V2, Offset7kNBits21A2)
{
    constexpr auto kExpected =
        "110011101101000101011001000010100110010110010001101010110010011011110101"sv;
    const auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{7}, kValue, NumBits{21});
    const auto kActual = std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected64V1, Offset6kNBits2A1)
{
    constexpr auto kExpected =
        "110011111101000101011001000010100110010110011011111000101010011111110101"sv;
    const auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{6}, kValue, NumBits{2});
    const auto kActual = std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected64V2, Offset6kNBits2A2)
{
    constexpr auto kExpected = k72SrcBitStr;
    const auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{6}, kValue, NumBits{2});
    const auto kActual = std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected64V1, ConstexprOffset6kNBits2A1)
{
    constexpr auto kExpected =
        "110011111101000101011001000010100110010110011011111000101010011111110101"sv;
    static constexpr auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{6}, kValue, NumBits{2});
    constexpr auto kActual =
        std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    static_assert(kActual == kExpected);
}

TEST_F(AddValueExpected64V2, ConstexprOffset6kNBits2A2)
{
    constexpr auto kExpected = k72SrcBitStr;
    static constexpr auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{6}, kValue, NumBits{2});
    constexpr auto kActual =
        std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    static_assert(kActual == kExpected);
}

TEST_F(AddValueExpected64V1, Offset5kNBits64A1)
{
    constexpr auto kExpected =
        "110011110010011110100110011001001101001000011010101100100110110100011101"sv;
    const auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{5}, kValue, NumBits{64});
    const auto kActual = std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected64V2, Offset5kNBits64A2)
{
    constexpr auto kExpected =
        "110111001001111010011001100100110100100001101010110010011011010001110101"sv;
    const auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{5}, kValue, NumBits{64});
    const auto kActual = std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(AddValueExpected64V1, ConstexprOffset5kNBits64A1)
{
    constexpr auto kExpected =
        "110011110010011110100110011001001101001000011010101100100110110100011101"sv;
    static constexpr auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{5}, kValue, NumBits{64});
    constexpr auto kActual =
        std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(AddValueExpected64V2, ConstexprOffset5kNBits64A2)
{
    constexpr auto kExpected =
        "110111001001111010011001100100110100100001101010110010011011010001110101"sv;
    static constexpr auto kActualArr = helpers::addValueExpected<kNBits>(
        k72SrcBitStr, DstOffset{5}, kValue, NumBits{64});
    constexpr auto kActual =
        std::string_view(kActualArr.data(), kActualArr.size());
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    static_assert(kActual == kExpected, "Invalid kActual.");
}
}  // namespace
