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
}  // namespace