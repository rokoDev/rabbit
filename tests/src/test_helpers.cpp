#include "test_helpers.h"

#include <gtest/gtest.h>

#include <array>
#include <string_view>
#include <utility>

#include "rabbit/utils.h"

namespace rabbit
{
std::string test_helpers::random_bit_sequence(const std::size_t aNBits)
{
    std::random_device rd;
    std::mt19937 mt(rd());
    std::uniform_int_distribution<uint32_t> dist(0, 1);

    std::string result;
    result.reserve(aNBits);
    for (std::size_t i = 0; i < aNBits; ++i)
    {
        result += dist(mt) ? '1' : '0';
    }
    return result;
}
}  // namespace rabbit

namespace
{
using namespace std::string_view_literals;
using helpers = rabbit::test_helpers;
using ::testing::Test;
using SrcBitOffset = rabbit::SrcBitOffset;
using NumBits = rabbit::NumBits;

class GetValueExpected : public Test
{
   protected:
    static inline constexpr auto k72SrcBitStr =
        "110011101101000101011001000010100110010110011011111000101010011111110101"sv;
};

TEST(ConvertBitStringView, ToBinArray1)
{
    constexpr auto kBitStr = ""sv;
    constexpr auto kResultArray =
        helpers::to_uint8_array<kBitStr.size()>(kBitStr);
    constexpr std::array<uint8_t, 0> kExpectedArray{};
    static_assert(rabbit::is_equal(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(ConvertBitStringView, ToBinArray2)
{
    constexpr auto kBitStr = "1"sv;
    constexpr auto kResultArray =
        helpers::to_uint8_array<kBitStr.size()>(kBitStr);
    constexpr std::array<uint8_t, 1> kExpectedArray{0b10000000};
    static_assert(rabbit::is_equal(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(ConvertBitStringView, ToBinArray3)
{
    constexpr auto kBitStr = "0"sv;
    constexpr auto kResultArray =
        helpers::to_uint8_array<kBitStr.size()>(kBitStr);
    constexpr std::array<uint8_t, 1> kExpectedArray{0b00000000};
    static_assert(rabbit::is_equal(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(ConvertBitStringView, ToBinArray4)
{
    constexpr auto kBitStr = "101"sv;
    constexpr auto kResultArray =
        helpers::to_uint8_array<kBitStr.size()>(kBitStr);
    constexpr std::array<uint8_t, 1> kExpectedArray{0b10100000};
    static_assert(rabbit::is_equal(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(ConvertBitStringView, ToBinArray5)
{
    constexpr auto kBitStr = "10100101"sv;
    constexpr auto kResultArray =
        helpers::to_uint8_array<kBitStr.size()>(kBitStr);
    constexpr std::array<uint8_t, 1> kExpectedArray{0b10100101};
    static_assert(rabbit::is_equal(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(ConvertBitStringView, ToBinArray6)
{
    constexpr auto kBitStr = "101001011"sv;
    constexpr auto kResultArray =
        helpers::to_uint8_array<kBitStr.size()>(kBitStr);
    constexpr std::array<uint8_t, 2> kExpectedArray{0b10100101, 0b10000000};
    static_assert(rabbit::is_equal(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(ConvertBitStringView, ToBinArray7)
{
    constexpr auto kBitStr = "10101010011111100100001"sv;
    constexpr auto kResultArray =
        helpers::to_uint8_array<kBitStr.size()>(kBitStr);
    constexpr std::array<uint8_t, 3> kExpectedArray{0b10101010, 0b01111110,
                                                    0b01000010};
    static_assert(rabbit::is_equal(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(ConvertBitStringView, ToBinArray8)
{
    constexpr auto kBitStr = "1010101001111110110000111"sv;
    constexpr auto kResultArray =
        helpers::to_uint8_array<kBitStr.size()>(kBitStr);
    constexpr std::array<uint8_t, 4> kExpectedArray{0b10101010, 0b01111110,
                                                    0b11000011, 0b10000000};
    static_assert(rabbit::is_equal(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(ConvertBinArray, ToBitStringView1)
{
    constexpr std::array<uint8_t, 4> kBitsArray{0b10101010, 0b01111110,
                                                0b11000011, 0b10000000};
    static constexpr auto kSymbols =
        helpers::to_symbol_array(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "10101010011111101100001110000000";
    static_assert(kResult == kExpected, "Invalid kResult.");
}

TEST(ConvertBinArray, ToBitStringView2)
{
    constexpr std::array<uint8_t, 4> kBitsArray{0b10101010, 0b01111110,
                                                0b11000011, 0b10000000};
    static constexpr auto kSymbols =
        helpers::to_symbol_array<25>(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "1010101001111110110000111";
    static_assert(kResult == kExpected, "Invalid kResult.");
}

TEST(ConvertBinArray, ToBitStringView3)
{
    constexpr std::array<uint8_t, 0> kBitsArray{};
    static constexpr auto kSymbols =
        helpers::to_symbol_array(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "";
    static_assert(kResult == kExpected, "Invalid kResult.");
}

TEST(ConvertBinArray, ToBitStringView4)
{
    constexpr std::array<uint8_t, 1> kBitsArray{0b10000000};
    static constexpr auto kSymbols =
        helpers::to_symbol_array(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "10000000";
    static_assert(kResult == kExpected, "Invalid kResult.");
}

TEST(ConvertBinArray, ToBitStringView5)
{
    constexpr std::array<uint8_t, 1> kBitsArray{0b10001100};
    static constexpr auto kSymbols =
        helpers::to_symbol_array<1>(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "1";
    static_assert(kResult == kExpected, "Invalid kResult.");
}

TEST(ConvertBinArray, ToBitStringView6)
{
    constexpr std::array<uint8_t, 1> kBitsArray{0b10001100};
    static constexpr auto kSymbols =
        helpers::to_symbol_array<5>(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "10001";
    static_assert(kResult == kExpected, "Invalid kResult.");
}

TEST_F(GetValueExpected, UInt8kOffset0kNBits0)
{
    using U = uint8_t;
    const U kExpected{0b00000000};
    const auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, SrcBitOffset{0}, NumBits{0});
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(GetValueExpected, UInt8kOffset0kNBits3)
{
    using U = uint8_t;
    const U kExpected{0b00000110};
    const auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, SrcBitOffset{0}, NumBits{3});
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(GetValueExpected, ConstexprUInt8kOffset3kNBits2)
{
    using U = uint8_t;
    constexpr U kExpected{0b00000001};
    constexpr SrcBitOffset kOffset{3};
    constexpr NumBits kNBits{2};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kNBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(GetValueExpected, ConstexprUInt8kOffset7kNBits8)
{
    using U = uint8_t;
    constexpr U kExpected{0b01101000};
    constexpr SrcBitOffset kOffset{7};
    constexpr NumBits kNBits{8};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kNBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(GetValueExpected, ConstexprUInt32kOffset7kNBits1)
{
    using U = uint32_t;
    constexpr U kExpected{0b00000000'00000000'00000000'00000000};
    constexpr SrcBitOffset kOffset{7};
    constexpr NumBits kNBits{1};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kNBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(GetValueExpected, ConstexprUInt32kOffset7kNBits21)
{
    using U = uint32_t;
    constexpr U kExpected{0b00000000'00001101'00010101'10010000};
    constexpr SrcBitOffset kOffset{7};
    constexpr NumBits kNBits{21};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kNBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(GetValueExpected, UInt32kOffset7kNBits21)
{
    using U = uint32_t;
    const U kExpected{0b00000000'00001101'00010101'10010000};
    const SrcBitOffset kOffset{7};
    const NumBits kNBits{21};
    const auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kNBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(GetValueExpected, UInt64kOffset6kNBits2)
{
    using U = uint64_t;
    const U kExpected{
        0b00000000'00000000'00000000'00000000'00000000'00000000'00000000'00000010};
    const SrcBitOffset kOffset{6};
    const NumBits kNBits{2};
    const auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kNBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(GetValueExpected, ConstexprUInt64kOffset6kNBits2)
{
    using U = uint64_t;
    constexpr U kExpected{
        0b00000000'00000000'00000000'00000000'00000000'00000000'00000000'00000010};
    constexpr SrcBitOffset kOffset{6};
    constexpr NumBits kNBits{2};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kNBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}

TEST_F(GetValueExpected, UInt64kOffset5kNBits64)
{
    using U = uint64_t;
    const U kExpected{
        0b11011010'00101011'00100001'01001100'10110011'01111100'01010100'11111110};
    const SrcBitOffset kOffset{5};
    const NumBits kNBits{64};
    const auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kNBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}

TEST_F(GetValueExpected, ConstexprUInt64kOffset5kNBits64)
{
    using U = uint64_t;
    constexpr U kExpected{
        0b11011010'00101011'00100001'01001100'10110011'01111100'01010100'11111110};
    constexpr SrcBitOffset kOffset{5};
    constexpr NumBits kNBits{64};
    constexpr auto kActual =
        helpers::getValueExpected<U>(k72SrcBitStr, kOffset, kNBits);
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    static_assert(kActual == kExpected, "Invalid kActual.");
}
}  // namespace