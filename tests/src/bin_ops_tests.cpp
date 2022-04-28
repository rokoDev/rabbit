#include <fmt/core.h>
#include <gtest/gtest.h>
#include <strong_type/strong_type.h>

#include <random>
#include <string>
#include <string_view>
#include <utility>

#include "buffer_tests.h"
#include "data_formatters_tests.h"
#include "rabbit/bin_ops.h"
#include "rabbit/endian.h"
#include "rabbit/utils.h"

using namespace std::string_view_literals;

template <std::size_t BufSize>
class TwoBufsTest : public ::testing::Test
{
   protected:
    Buffer<BufSize> dst_;
    Buffer<BufSize> src_;
};

template <std::size_t BufSize>
class BinOpsTest : public ::testing::Test
{
   public:
    BinOpsTest() : rawData_{} {}

    void print() { fmt::print("{}\n", to_string()); }

    std::string to_string() const noexcept
    {
        return to_string_impl(std::make_index_sequence<BufSize>{});
    }

   protected:
    template <std::size_t... I>
    std::string to_string_impl(std::index_sequence<I...>) const noexcept
    {
        return fmt::format(BinFormatStringView<BufSize>, rawData_[I]...);
    }

    std::array<uint8_t, BufSize> rawData_{};
};

using N1BinOpsTest = BinOpsTest<sizeof(uint8_t)>;
using N2BinOpsTest = BinOpsTest<sizeof(uint16_t)>;
using N3BinOpsTest = BinOpsTest<3>;
using N4BinOpsTest = BinOpsTest<sizeof(uint32_t)>;
using N8BinOpsTest = BinOpsTest<sizeof(uint64_t)>;
using N32BinOpsTest = BinOpsTest<32>;

using N1TwoBufsTest = TwoBufsTest<1>;
using N2TwoBufsTest = TwoBufsTest<2>;
using N3TwoBufsTest = TwoBufsTest<3>;
using N4TwoBufsTest = TwoBufsTest<4>;
using N8TwoBufsTest = TwoBufsTest<8>;
using N32TwoBufsTest = TwoBufsTest<32>;

namespace bin_op = rabbit::details;

namespace
{
TEST(BinOpsTest, Mask1)
{
    constexpr auto kMask = bin_op::mask<uint8_t>(2, 3);
    constexpr uint8_t expectedMask{0b00111000};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, InvertedMask1)
{
    constexpr auto kMask = bin_op::invertedMask<uint8_t>(2, 3);
    constexpr uint8_t expectedMask{0b11000111};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, Mask2)
{
    constexpr auto kMask = bin_op::mask<uint16_t>(6, 5);
    constexpr uint16_t expectedMask{0b0000001111100000};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, InvertedMask2)
{
    constexpr auto kMask = bin_op::invertedMask<uint16_t>(6, 5);
    constexpr uint16_t expectedMask{0b1111110000011111};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, Mask3)
{
    constexpr auto kMask = bin_op::mask<uint32_t>(7, 10);
    constexpr uint32_t expectedMask{0b00000001111111111000000000000000};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, InvertedMask3)
{
    constexpr auto kMask = bin_op::invertedMask<uint32_t>(7, 10);
    constexpr uint32_t expectedMask{0b11111110000000000111111111111111};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, ByteAtUInt32t)
{
    uint32_t value = 0b00001011'00010110'00100001'00101100;
    ASSERT_EQ(bin_op::byteAt<0>(value), 0b00001011);
    ASSERT_EQ(bin_op::byteAt<1>(value), 0b00010110);
    ASSERT_EQ(bin_op::byteAt<2>(value), 0b00100001);
    ASSERT_EQ(bin_op::byteAt<3>(value), 0b00101100);
}

template <typename T>
constexpr decltype(auto) check_highNBits(T &&aValue, uint8_t aNBits) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(rabbit::is_uint_v<UIntT>,
                  "UIntT must be unsigned integer and not bool.");
    auto result = bin_op::highNBits(std::forward<T>(aValue), aNBits);
    static_assert(std::is_same_v<decltype(result), UIntT>,
                  "Deduced type is invalid.");
    return result;
}

template <typename T>
constexpr decltype(auto) check_lowNBits(T &&aValue, uint8_t aNBits) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(rabbit::is_uint_v<UIntT>,
                  "UIntT must be unsigned integer and not bool.");
    auto result = bin_op::lowNBits(std::forward<T>(aValue), aNBits);
    static_assert(std::is_same_v<decltype(result), UIntT>,
                  "Deduced type is invalid.");
    return result;
}

TEST(BinOpsTest, highNBits8_0)
{
    using U = uint8_t;
    ASSERT_EQ(check_highNBits(U{0b10100101}, 0), U{0b00000000});
}

TEST(BinOpsTest, highNBits8_3)
{
    using U = uint8_t;
    ASSERT_EQ(check_highNBits(U{0b10100101}, 3), U{0b10100000});
}

TEST(BinOpsTest, highNBits8_8)
{
    using U = uint8_t;
    ASSERT_EQ(check_highNBits(U{0b10100101}, 8), U{0b10100101});
}

TEST(BinOpsTest, highNBits16_3)
{
    using U = uint16_t;
    ASSERT_EQ(check_highNBits(U{0b10100101'11111111}, 3),
              U{0b10100000'00000000});
}

TEST(BinOpsTest, highNBits16_8)
{
    using U = uint16_t;
    ASSERT_EQ(check_highNBits(U{0b10100101'11111111}, 8),
              U{0b10100101'00000000});
}

TEST(BinOpsTest, highNBits16_10)
{
    using U = uint16_t;
    ASSERT_EQ(check_highNBits(U{0b10100101'11111111}, 10),
              U{0b10100101'11000000});
}

TEST(BinOpsTest, highNBits32_25)
{
    using U = uint32_t;
    ASSERT_EQ(check_highNBits(U{0b10100101'11111111'01010101'10101010}, 25),
              U{0b10100101'11111111'01010101'10000000});
}

TEST(BinOpsTest, highNBits64_45)
{
    using U = uint64_t;
    constexpr U origin{
        0b10100101'11111111'01010101'10101010'10100101'11111111'01010101'10101010};
    constexpr U supposedResult{
        0b10100101'11111111'01010101'10101010'10100101'11111000'00000000'00000000};
    ASSERT_EQ(check_highNBits(origin, 45), supposedResult);
}

TEST(BinOpsTest, lowNBits8_0)
{
    using U = uint8_t;
    ASSERT_EQ(check_lowNBits(U{0b10100101}, 0), U{0b00000000});
}

TEST(BinOpsTest, lowNBits8_3)
{
    using U = uint8_t;
    ASSERT_EQ(check_lowNBits(U{0b10100101}, 3), U{0b00000101});
}

TEST(BinOpsTest, lowNBits8_8)
{
    using U = uint8_t;
    ASSERT_EQ(check_lowNBits(U{0b10100101}, 8), U{0b10100101});
}

TEST(BinOpsTest, lowNBits16_3)
{
    using U = uint16_t;
    ASSERT_EQ(check_lowNBits(U{0b10100101'11111111}, 3),
              U{0b00000000'00000111});
}

TEST(BinOpsTest, lowNBits16_8)
{
    using U = uint16_t;
    ASSERT_EQ(check_lowNBits(U{0b10100101'11111111}, 8),
              U{0b00000000'11111111});
}

TEST(BinOpsTest, lowNBits16_10)
{
    using U = uint16_t;
    ASSERT_EQ(check_lowNBits(U{0b10100101'11111111}, 10),
              U{0b00000001'11111111});
}

TEST(BinOpsTest, lowNBits32_25)
{
    using U = uint32_t;
    ASSERT_EQ(check_lowNBits(U{0b10100101'11111111'01010101'10101010}, 25),
              U{0b00000001'11111111'01010101'10101010});
}

TEST(BinOpsTest, lowNBits64_45)
{
    using U = uint64_t;
    constexpr U origin{
        0b10100101'11111111'01010101'10101010'10100101'11111111'01010101'10101010};
    constexpr U supposedResult{
        0b00000000'00000000'00010101'10101010'10100101'11111111'01010101'10101010};
    ASSERT_EQ(check_lowNBits(origin, 45), supposedResult);
}

TEST_F(N1BinOpsTest, addValue8)
{
    using U = uint8_t;
    using Indices = std::make_index_sequence<sizeof(U)>;
    constexpr U valueToAdd{172};
    bin_op::addValue(rawData_.data(), valueToAdd, Indices{});
    ASSERT_EQ(rawData_[0], valueToAdd);
}

TEST_F(N2BinOpsTest, addValue8)
{
    using U = uint8_t;
    using Indices = std::make_index_sequence<sizeof(U)>;
    constexpr U valueToAdd{172};
    constexpr uint8_t secondValue{0b11110111};
    rawData_[1] = secondValue;
    bin_op::addValue(rawData_.data(), valueToAdd, Indices{});
    ASSERT_EQ(rawData_[0], valueToAdd);
    ASSERT_EQ(rawData_[1], secondValue);
}

TEST_F(N2BinOpsTest, addValue16)
{
    using U = uint16_t;
    using Indices = std::make_index_sequence<sizeof(U)>;
    constexpr U valueToAdd{0b00010000'11111111};
    bin_op::addValue(rawData_.data(), valueToAdd, Indices{});
    ASSERT_EQ(rawData_[0], uint8_t{0b00010000});
    ASSERT_EQ(rawData_[1], uint8_t{0b11111111});
}

TEST_F(N4BinOpsTest, addValue32)
{
    using U = uint32_t;
    using Indices = std::make_index_sequence<sizeof(U)>;
    constexpr U valueToAdd{0b10101010'10100101'00010000'11111111};
    bin_op::addValue(rawData_.data(), valueToAdd, Indices{});
    ASSERT_EQ(rawData_[0], uint8_t{0b10101010});
    ASSERT_EQ(rawData_[1], uint8_t{0b10100101});
    ASSERT_EQ(rawData_[2], uint8_t{0b00010000});
    ASSERT_EQ(rawData_[3], uint8_t{0b11111111});
}

TEST_F(N4BinOpsTest, addValue32Shifted)
{
    using U = uint32_t;
    using Indices =
        rabbit::shifted_sequence_t<1, std::make_index_sequence<sizeof(U) - 1>>;
    constexpr U valueToAdd{0b10101010'10100101'00010000'11111111};
    bin_op::addValue(rawData_.data(), valueToAdd, Indices{});
    ASSERT_EQ(rawData_[0], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[1], uint8_t{0b10100101});
    ASSERT_EQ(rawData_[2], uint8_t{0b00010000});
    ASSERT_EQ(rawData_[3], uint8_t{0b11111111});
}

TEST_F(N8BinOpsTest, addValue64)
{
    using U = uint64_t;
    using Indices = std::make_index_sequence<sizeof(U)>;
    constexpr U valueToAdd{
        0b10100101'11111111'01010101'10101010'10100101'11111111'01010101'10101010};
    bin_op::addValue(rawData_.data(), valueToAdd, Indices{});
    ASSERT_EQ(rawData_[0], uint8_t{0b10100101});
    ASSERT_EQ(rawData_[1], uint8_t{0b11111111});
    ASSERT_EQ(rawData_[2], uint8_t{0b01010101});
    ASSERT_EQ(rawData_[3], uint8_t{0b10101010});
    ASSERT_EQ(rawData_[4], uint8_t{0b10100101});
    ASSERT_EQ(rawData_[5], uint8_t{0b11111111});
    ASSERT_EQ(rawData_[6], uint8_t{0b01010101});
    ASSERT_EQ(rawData_[7], uint8_t{0b10101010});
}

TEST_F(N8BinOpsTest, addUInt64High)
{
    using U = uint64_t;
    rawData_[0] = uint8_t{0b11111111};
    constexpr U valueToAdd{
        0b00000101'11111111'01010101'10101010'10100101'11111111'01010101'10101010};
    bin_op::addUInt64High(rawData_.data(), 59, valueToAdd);
    ASSERT_EQ(rawData_[0], uint8_t{0b11111101});
    ASSERT_EQ(rawData_[1], uint8_t{0b11111111});
    ASSERT_EQ(rawData_[2], uint8_t{0b01010101});
    ASSERT_EQ(rawData_[3], uint8_t{0b10101010});
    ASSERT_EQ(rawData_[4], uint8_t{0b10100101});
    ASSERT_EQ(rawData_[5], uint8_t{0b11111111});
    ASSERT_EQ(rawData_[6], uint8_t{0b01010101});
    ASSERT_EQ(rawData_[7], uint8_t{0b10101010});
}

TEST_F(N1BinOpsTest, addUInt64Low)
{
    using U = uint8_t;
    constexpr U valueToAdd{0b00011101};
    bin_op::addUInt64Low(rawData_.data(), 5, valueToAdd);
    ASSERT_EQ(rawData_[0], uint8_t{0b11101000});
}

TEST_F(N1BinOpsTest, addUInt8WithoutOffset)
{
    using U = uint8_t;
    constexpr U kValue = 0b10101111;
    bin_op::add(rawData_.data(), kValue);
    ASSERT_EQ(rawData_[0], kValue);
}

TEST_F(N2BinOpsTest, addUInt8WithoutOffset)
{
    using U = uint8_t;
    constexpr U kValue = 0b10101111;
    bin_op::add(rawData_.data(), kValue);
    ASSERT_EQ(rawData_[0], kValue);
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, addUInt16WithoutOffset)
{
    using U = uint16_t;
    constexpr U kValue = 0b10101111'00010111;
    bin_op::add(rawData_.data(), kValue);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101111});
    ASSERT_EQ(rawData_[1], uint8_t{0b00010111});
}

TEST_F(N3BinOpsTest, addUInt16WithoutOffset)
{
    using U = uint16_t;
    constexpr U kValue = 0b10101111'00010111;
    bin_op::add(rawData_.data() + 1, kValue);
    ASSERT_EQ(rawData_[0], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[1], uint8_t{0b10101111});
    ASSERT_EQ(rawData_[2], uint8_t{0b00010111});
}

TEST_F(N4BinOpsTest, addUInt32WithoutOffset)
{
    using U = uint32_t;
    constexpr U kValue = 0b10101111'00010111'00001111'00110011;
    bin_op::add(rawData_.data(), kValue);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101111});
    ASSERT_EQ(rawData_[1], uint8_t{0b00010111});
    ASSERT_EQ(rawData_[2], uint8_t{0b00001111});
    ASSERT_EQ(rawData_[3], uint8_t{0b00110011});
}

TEST_F(N8BinOpsTest, addUInt32WithoutOffset)
{
    using U = uint32_t;
    constexpr U kValue = 0b10101111'00010111'00001111'00110011;
    bin_op::add(rawData_.data() + 2, kValue);
    ASSERT_EQ(rawData_[0], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[2], uint8_t{0b10101111});
    ASSERT_EQ(rawData_[3], uint8_t{0b00010111});
    ASSERT_EQ(rawData_[4], uint8_t{0b00001111});
    ASSERT_EQ(rawData_[5], uint8_t{0b00110011});
    ASSERT_EQ(rawData_[6], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[7], uint8_t{0b00000000});
}

TEST_F(N8BinOpsTest, addUInt64WithoutOffset)
{
    using U = uint64_t;
    constexpr U kValue =
        0b10101111'00010111'00001111'00110011'10101010'01111110'10000001'10011001;
    bin_op::add(rawData_.data(), kValue);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101111});
    ASSERT_EQ(rawData_[1], uint8_t{0b00010111});
    ASSERT_EQ(rawData_[2], uint8_t{0b00001111});
    ASSERT_EQ(rawData_[3], uint8_t{0b00110011});
    ASSERT_EQ(rawData_[4], uint8_t{0b10101010});
    ASSERT_EQ(rawData_[5], uint8_t{0b01111110});
    ASSERT_EQ(rawData_[6], uint8_t{0b10000001});
    ASSERT_EQ(rawData_[7], uint8_t{0b10011001});
}

TEST_F(N8BinOpsTest, AddNLeastSignificantBytes)
{
    using U = uint64_t;
    constexpr U kValue =
        0b10101111'00010111'00001111'00110011'10101010'01111110'10000001'10011001;
    bin_op::addNLeastSignificantBytes<const U &&, 3>(rawData_.data(),
                                                     std::move(kValue));
    ASSERT_EQ(rawData_[0], uint8_t{0b01111110});
    ASSERT_EQ(rawData_[1], uint8_t{0b10000001});
    ASSERT_EQ(rawData_[2], uint8_t{0b10011001});
    ASSERT_EQ(rawData_[3], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[4], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[5], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[6], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[7], uint8_t{0b00000000});
}

TEST_F(N4BinOpsTest, AddNLeastSignificantBytes)
{
    using U = uint8_t;
    constexpr U kValue = 0b10101111;
    bin_op::addNLeastSignificantBytes<const U &&, 1>(rawData_.data(),
                                                     std::move(kValue));
    ASSERT_EQ(rawData_[0], uint8_t{0b10101111});
}

TEST_F(N4BinOpsTest, AddZeroLeastSignificantBytes)
{
    using U = uint64_t;
    constexpr U kValue =
        0b10101111'00010111'00001111'00110011'10101010'01111110'10000001'10011001;
    bin_op::addNLeastSignificantBytes<const U &&, 0>(rawData_.data(),
                                                     std::move(kValue));
    ASSERT_EQ(rawData_[0], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[2], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[3], uint8_t{0b00000000});
}

TEST(BinOpsTest, highNBitsWith2Offset)
{
    using U = uint16_t;
    constexpr U kValue = 0b10011011'10101010;
    constexpr uint8_t kDstOffset = 2;
    constexpr uint8_t kNBits = 4;
    constexpr auto high4Bits =
        bin_op::highNBitsWithOffset(kValue, kNBits, kDstOffset);
    static_assert(std::is_same_v<std::remove_const_t<decltype(high4Bits)>, U>,
                  "high4Bits is of invalid type");
    ASSERT_EQ(high4Bits, U{0b00100100'00000000});
}

TEST(BinOpsTest, highNBitsWith0Offset)
{
    using U = uint16_t;
    constexpr U kValue = 0b10011011'10101010;
    constexpr uint8_t kDstOffset = 0;
    constexpr uint8_t kNBits = 4;
    constexpr auto high4Bits =
        bin_op::highNBitsWithOffset(kValue, kNBits, kDstOffset);
    static_assert(std::is_same_v<std::remove_const_t<decltype(high4Bits)>, U>,
                  "high4Bits is of invalid type");
    ASSERT_EQ(high4Bits, U{0b10010000'00000000});
}

TEST(BinOpsTest, highNBitsWith10Offset)
{
    using U = uint16_t;
    constexpr U kValue = 0b10011011'10101010;
    constexpr uint8_t kDstOffset = 10;
    constexpr uint8_t kNBits = 4;
    constexpr auto high4Bits =
        bin_op::highNBitsWithOffset(kValue, kNBits, kDstOffset);
    static_assert(std::is_same_v<std::remove_const_t<decltype(high4Bits)>, U>,
                  "high4Bits is of invalid type");
    ASSERT_EQ(high4Bits, U{0b00000000'00100100});
}

TEST(BinOpsTest, highNBitsWith12Offset)
{
    using U = uint16_t;
    constexpr U kValue = 0b10011011'10101010;
    constexpr uint8_t kDstOffset = 12;
    constexpr uint8_t kNBits = 4;
    constexpr auto high4Bits =
        bin_op::highNBitsWithOffset(kValue, kNBits, kDstOffset);
    static_assert(std::is_same_v<std::remove_const_t<decltype(high4Bits)>, U>,
                  "high4Bits is of invalid type");
    ASSERT_EQ(high4Bits, U{0b00000000'00001001});
}

TEST_F(N4BinOpsTest, GetUInt16From1Byte)
{
    constexpr std::size_t kNBytes = 1;
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    const auto value =
        bin_op::get<uint16_t, bin_op::eAlign::kLeft>(rawData_.data(), kNBytes);
    static_assert(
        std::is_same_v<std::remove_const_t<decltype(value)>, uint16_t>,
        "value is of invalid type.");
    ASSERT_EQ(bin_op::byteAt<0>(value), uint8_t{0b10101111});
}

TEST_F(N4BinOpsTest, GetUInt16From2Bytes)
{
    constexpr std::size_t kNBytes = 2;
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    const auto value =
        bin_op::get<uint16_t, bin_op::eAlign::kLeft>(rawData_.data(), kNBytes);
    static_assert(
        std::is_same_v<std::remove_const_t<decltype(value)>, uint16_t>,
        "value is of invalid type.");
    ASSERT_EQ(value, uint16_t{0b10101111'10000001});
}

TEST_F(N4BinOpsTest, GetUInt32From3Bytes)
{
    constexpr std::size_t kNBytes = 3;
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    const auto value =
        bin_op::get<uint32_t, bin_op::eAlign::kLeft>(rawData_.data(), kNBytes);
    static_assert(
        std::is_same_v<std::remove_const_t<decltype(value)>, uint32_t>,
        "value is of invalid type.");
    ASSERT_EQ(bin_op::byteAt<0>(value), uint8_t{0b10101111});
    ASSERT_EQ(bin_op::byteAt<1>(value), uint8_t{0b10000001});
    ASSERT_EQ(bin_op::byteAt<2>(value), uint8_t{0b10011001});
}

TEST_F(N4BinOpsTest, GetUInt32From4Bytes)
{
    constexpr std::size_t kNBytes = 4;
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    const auto value =
        bin_op::get<uint32_t, bin_op::eAlign::kLeft>(rawData_.data(), kNBytes);
    static_assert(
        std::is_same_v<std::remove_const_t<decltype(value)>, uint32_t>,
        "value is of invalid type.");
    ASSERT_EQ(value, uint32_t{0b10101111'10000001'10011001'00111100});
}

TEST_F(N1TwoBufsTest, AddHighBits0)
{
    src_[0] = 0b01101110;
    dst_[0] = 0b11101110;
    dst_[0] = bin_op::addHighBits(*dst_.data(), *src_.data(), 0);
    ASSERT_EQ(dst_[0], uint8_t{0b11101110});
}

TEST_F(N1TwoBufsTest, AddHighBits1)
{
    src_[0] = 0b01101010;
    dst_[0] = 0b11101110;
    dst_[0] = bin_op::addHighBits(*dst_.data(), *src_.data(), 1);
    ASSERT_EQ(dst_[0], uint8_t{0b01101110});
}

TEST_F(N1TwoBufsTest, AddHighBits2)
{
    src_[0] = 0b01101010;
    dst_[0] = 0b00101110;
    dst_[0] = bin_op::addHighBits(*dst_.data(), *src_.data(), 2);
    ASSERT_EQ(dst_[0], uint8_t{0b01101110});
}

TEST_F(N1TwoBufsTest, AddHighBits7)
{
    src_[0] = 0b01101011;
    dst_[0] = 0b00101110;
    dst_[0] = bin_op::addHighBits(*dst_.data(), *src_.data(), 7);
    ASSERT_EQ(dst_[0], uint8_t{0b01101010});
}

TEST_F(N1TwoBufsTest, AddHighBits8)
{
    src_[0] = 0b01101011;
    dst_[0] = 0b00101110;
    dst_[0] = bin_op::addHighBits(*dst_.data(), *src_.data(), 8);
    ASSERT_EQ(dst_[0], uint8_t{0b01101011});
}

TEST_F(N1TwoBufsTest, AddLowBits0)
{
    src_[0] = 0b01101111;
    dst_[0] = 0b00101110;
    dst_[0] = bin_op::addLowBits(*dst_.data(), *src_.data(), 0);
    ASSERT_EQ(dst_[0], uint8_t{0b00101110});
}

TEST_F(N1TwoBufsTest, AddLowBits1)
{
    src_[0] = 0b11101111;
    dst_[0] = 0b00101110;
    dst_[0] = bin_op::addLowBits(*dst_.data(), *src_.data(), 1);
    ASSERT_EQ(dst_[0], uint8_t{0b00101111});
}

TEST_F(N1TwoBufsTest, AddLowBits2)
{
    src_[0] = 0b01101110;
    dst_[0] = 0b10101001;
    dst_[0] = bin_op::addLowBits(*dst_.data(), *src_.data(), 2);
    ASSERT_EQ(dst_[0], uint8_t{0b10101010});
}

TEST_F(N1TwoBufsTest, AddLowBits7)
{
    src_[0] = 0b01101110;
    dst_[0] = 0b10101001;
    dst_[0] = bin_op::addLowBits(*dst_.data(), *src_.data(), 7);
    ASSERT_EQ(dst_[0], uint8_t{0b11101110});
}

TEST_F(N1TwoBufsTest, AddLowBits8)
{
    src_[0] = 0b01101110;
    dst_[0] = 0b10101001;
    dst_[0] = bin_op::addLowBits(*dst_.data(), *src_.data(), 8);
    ASSERT_EQ(dst_[0], uint8_t{0b01101110});
}
}  // namespace