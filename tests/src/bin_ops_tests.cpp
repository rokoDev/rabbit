#include <fmt/core.h>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <strong_type/strong_type.h>

#include <random>
#include <string>
#include <string_view>
#include <utility>

#include "rabbit/bin_ops.h"
#include "rabbit/endian.h"
#include "rabbit/utils.h"

namespace details
{
template <std::size_t ByteIndex, std::size_t Size, std::size_t Len1,
          std::size_t Len2, std::size_t... I1, std::size_t... I2>
constexpr decltype(auto) format_data_array(const char (&aArr1)[Len1],
                                           std::index_sequence<I1...>,
                                           const char (&aArr2)[Len2],
                                           std::index_sequence<I2...>) noexcept
{
    if constexpr (ByteIndex < Size - 1)
    {
        return rabbit::concatenate_arrays(rabbit::make_array(aArr1[I1]...),
                                          rabbit::make_array(aArr2[I2]...));
    }
    else
    {
        return rabbit::make_array(aArr1[I1]...);
    }
}

template <std::size_t Len1, std::size_t Len2, std::size_t... I>
constexpr decltype(auto) make_fmt_data_array(const char (&aArr1)[Len1],
                                             const char (&aArr2)[Len2],
                                             std::index_sequence<I...>) noexcept
{
    constexpr std::size_t Size = sizeof...(I);
    using Indices1 = std::make_index_sequence<Len1 - 1>;
    using Indices2 = std::make_index_sequence<Len2 - 1>;
    return rabbit::concatenate_arrays(
        format_data_array<I, Size>(aArr1, Indices1{}, aArr2, Indices2{})...);
}

template <std::size_t NBytes>
struct BinFormatData
{
    static constexpr auto formatDataArray =
        make_fmt_data_array("{:08b}", " ", std::make_index_sequence<NBytes>{});
    static constexpr std::string_view formatStringView{formatDataArray.data(),
                                                       formatDataArray.size()};
};
}  // namespace details

template <std::size_t NBytes>
constexpr std::string_view BinFormatStringView =
    details::BinFormatData<NBytes>::formatStringView;

template <std::size_t BufSize>
class Buffer
{
   public:
    template <typename T, typename... U>
    constexpr Buffer(T &&aFirstArg, U &&... aRestArgs) noexcept
        : array_{rabbit::concatenate_arrays(
              rabbit::make_array(static_cast<uint8_t>(aFirstArg),
                                 static_cast<uint8_t>(aRestArgs)...),
              rabbit::make_zero_array<uint8_t,
                                      BufSize - sizeof...(aRestArgs) - 1>())}
    {
    }

    void print() { fmt::print("{}\n", to_string()); }

    Buffer() = default;

    std::string to_string() const noexcept
    {
        return to_string_impl(std::make_index_sequence<BufSize>{});
    }

    constexpr decltype(auto) operator[](std::size_t aPos)
    {
        return array_[aPos];
    }
    constexpr decltype(auto) operator[](std::size_t aPos) const
    {
        return array_[aPos];
    }

    constexpr uint8_t *data() noexcept { return array_.data(); }

    constexpr uint8_t const *data() const noexcept { return array_.data(); }

    std::array<uint8_t, BufSize> array_{};

   private:
    template <std::size_t... I>
    std::string to_string_impl(std::index_sequence<I...>) const noexcept
    {
        return fmt::format(BinFormatStringView<BufSize>, array_[I]...);
    }
};

template <std::size_t BufSize>
class TwoBufsTest : public ::testing::Test
{
   protected:
    Buffer<BufSize> dst_;
    Buffer<BufSize> src_;
};

template <>
class TwoBufsTest<32> : public ::testing::Test
{
   protected:
    Buffer<32> dst_ =
        Buffer<32>(0b10101111, 0b00010111, 0b00001111, 0b00110011, 0b10101010,
                   0b01111110, 0b10000001, 0b10011001, 0b10111101, 0b00111100);
    Buffer<32> src_;
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
        return fmt::format(formatStr_, rawData_[I]...);
    }

    std::array<uint8_t, BufSize> rawData_{};
    static constexpr auto formatDataArray_ = details::make_fmt_data_array(
        "{:08b}", " ", std::make_index_sequence<BufSize>{});
    static constexpr std::string_view formatStr_{formatDataArray_.data(),
                                                 formatDataArray_.size()};
};

class Converter
{
   public:
    template <std::size_t NBits, typename T>
    static constexpr decltype(auto) to_uint8_array(T &&aBitStr) noexcept
    {
        constexpr std::size_t NBytes =
            NBits / CHAR_BIT + (NBits % CHAR_BIT ? 1 : 0);
        return to_uint8_array_impl<NBits>(aBitStr,
                                          std::make_index_sequence<NBytes>{});
    }

    template <std::size_t N, typename T>
    static constexpr decltype(auto) to_uint8_array(T (&aBitStr)[N]) noexcept
    {
        static_assert(N > 0, "Condition N > 0 must be satisfied.");
        return to_uint8_array<N - 1>(aBitStr);
    }

    template <std::size_t NBits, typename ArrayT>
    static constexpr decltype(auto) to_symbol_array(ArrayT &&aArray) noexcept
    {
        using namespace rabbit;
        static_assert(std::is_same_v<std_array_data_t<ArrayT>, uint8_t>,
                      "Elements of aArray must belong to uint8_t type.");
        constexpr std::size_t kMaxNBytes = std_array_size_v<ArrayT>;
        static_assert(NBits <= kMaxNBytes * CHAR_BIT, "NBits is too large.");
        constexpr std::size_t kNBytes =
            NBits / CHAR_BIT + ((NBits % CHAR_BIT) ? 1 : 0);
        return to_symbol_array_impl<NBits>(std::forward<ArrayT>(aArray),
                                           std::make_index_sequence<kNBytes>{});
    }

    template <typename ArrayT>
    static constexpr decltype(auto) to_symbol_array(ArrayT &&aArray) noexcept
    {
        constexpr std::size_t kNBytes = rabbit::std_array_size_v<ArrayT>;
        return to_symbol_array<kNBytes * CHAR_BIT>(
            std::forward<ArrayT>(aArray));
    }

   private:
    template <std::size_t ByteIndex, typename RandAccessContainerT,
              std::size_t... I>
    static constexpr uint8_t to_uint8_impl(RandAccessContainerT &&aBitStr,
                                           std::index_sequence<I...>) noexcept
    {
        static_assert(sizeof...(I) > 0, "Bits count cannot be zero.");
        static_assert(sizeof...(I) <= CHAR_BIT,
                      "Bits count cannot be more than bits in a byte.");
        return static_cast<uint8_t>(
            (... | ((aBitStr[ByteIndex * CHAR_BIT + I] == '0')
                        ? 0
                        : (1 << (CHAR_BIT - I - 1)))));
    }

    template <std::size_t NBits, typename RandAccessContainerT,
              std::size_t... I>
    static constexpr decltype(auto) to_uint8_array_impl(
        RandAccessContainerT &&aBitStr, std::index_sequence<I...>) noexcept
    {
        return rabbit::make_array<uint8_t>(to_uint8_impl<I>(
            aBitStr,
            std::make_index_sequence<(((I + 1) * CHAR_BIT) <= NBits
                                          ? CHAR_BIT
                                          : (NBits - I * CHAR_BIT))>{})...);
    }

    template <std::size_t... I>
    static constexpr decltype(auto) to_symbol_array(
        const uint8_t aValue, std::index_sequence<I...>) noexcept
    {
        static_assert(sizeof...(I) <= CHAR_BIT, "Bit count is too large.");
        return std::array<char, sizeof...(I)>{
            ((aValue & (1 << (CHAR_BIT - 1 - I))) ? '1' : '0')...};
    }

    template <std::size_t NBits, typename ArrayT, std::size_t... I>
    static constexpr decltype(auto) to_symbol_array_impl(
        ArrayT &&aArray, std::index_sequence<I...>) noexcept
    {
        return rabbit::concatenate_arrays(
            rabbit::make_array<char>(),
            to_symbol_array(
                aArray[I],
                std::make_index_sequence<(((I + 1) * CHAR_BIT <= NBits)
                                              ? CHAR_BIT
                                              : NBits % CHAR_BIT)>{})...);
    }
};

class BitStrOps
{
   public:
    template <std::size_t DstOffset, std::size_t SrcOffset, std::size_t NDst,
              std::size_t NSrc, std::size_t NBits>
    static constexpr decltype(auto) addBitsResult(
        std::string_view aDst, std::string_view aSrc) noexcept
    {
        static_assert(DstOffset + NBits <= NDst, "Invalid input parameters.");
        static_assert(SrcOffset + NBits <= NSrc, "Invalid input parameters.");
        using namespace rabbit;

        const auto kPrefixArray =
            subarray(aDst, std::make_index_sequence<DstOffset>{});

        const auto kNBitsStr = aSrc.substr(SrcOffset, NBits);
        const auto kNBitsArray =
            subarray(std::move(kNBitsStr), std::make_index_sequence<NBits>{});

        const auto kSuffixStr =
            aDst.substr(DstOffset + NBits, NDst - DstOffset - NBits);
        const auto kSuffixArray =
            subarray(std::move(kSuffixStr),
                     std::make_index_sequence<NDst - DstOffset - NBits>{});

        const auto kResultStr =
            concatenate_arrays(std::move(kPrefixArray), std::move(kNBitsArray),
                               std::move(kSuffixArray));
        return Converter::to_uint8_array<NDst>(std::move(kResultStr));
    }

   private:
    template <typename ArrT, std::size_t... I>
    static constexpr decltype(auto) subarray(ArrT &&aArray,
                                             std::index_sequence<I...>) noexcept
    {
        using namespace rabbit;
        return std::array<typename std::decay_t<ArrT>::value_type,
                          sizeof...(I)>{aArray[I]...};
    }
};

std::string random_bit_sequence(const std::size_t aNBits)
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

class BinOpsAddBits : public ::testing::Test
{
   public:
    static constexpr std::string_view kSrc256{
        "1001100111011010001010110010000101001100101100110111110001010100111101"
        "0101011110111010101001010011001100000111101001000011111101001100101100"
        "1010101010111010100110011100100110101110110000011100100000111011100111"
        "0101110111011111110000001001001010100111100101"};

    static constexpr std::string_view kDst256{
        "0011011100100111101001100110010011010010000110101011001001101101000110"
        "0000000111010001110111111011000010010100100111111001011100111000011010"
        "1101011011110101101101101011101010011100111010110000000001011000100001"
        "0111110000110100100101111111100011000111000010"};

   protected:
    static constexpr std::size_t minBytesCount(
        const std::size_t aOffset, const std::size_t aNBits) noexcept
    {
        const std::size_t kAllBits = aOffset + aNBits;
        return kAllBits / CHAR_BIT + (kAllBits % CHAR_BIT ? 1 : 0);
    }
};

// TEST_F(BinOpsAddBits, Test1Bits)
//{
//    constexpr std::size_t SrcOffset = 3;
//    constexpr std::size_t DstOffset = 0;
//    constexpr std::size_t NBits = 10;
//
//    constexpr std::size_t kBitsInSrc = minBytesCount(SrcOffset, NBits);
//    constexpr std::size_t kBitsInDst = minBytesCount(DstOffset, NBits);
//
//    using namespace std::string_view_literals;
//    constexpr auto kSrc = kDst256.substr(0, kBitsInSrc);
//    constexpr auto kDst = kDst256.substr(0, kBitsInDst);
//
//    constexpr auto kExcpected =
//        BitStrOps::addBitsResult<DstOffset, SrcOffset, kDst.size(),
//        kSrc.size(),
//                                 NBits>(kDst, kSrc);
//
//    auto dstBuf = Converter::to_uint8_array<kDst.size()>(kDst);
//    constexpr auto kSrcBuf = Converter::to_uint8_array<kSrc.size()>(kSrc);
//    constexpr rabbit::DstBitOffset kDstOffset{DstOffset};
//    constexpr rabbit::SrcBitOffset kSrcOffset{SrcOffset};
//    constexpr rabbit::NumBits kNBits{NBits};
//    rabbit::BinOps::addBits(dstBuf.data(), kDstOffset, kSrcBuf.data(),
//                            kSrcOffset, kNBits);
//    ASSERT_TRUE(rabbit::is_equal_arrays(dstBuf, kExcpected));
//}

TEST(Converter, TestDataSet)
{
    using namespace std::string_view_literals;
    constexpr auto src256 =
        "1001100111011010001010110010000101001100101100110111110001010100111101010101111011101010100101001100110000011110100100001111110100110010110010101010101110101001100111001001101011101100000111001000001110111001110101110111011111110000001001001010100111100101"sv;
    static_assert(src256.size() == 256, "Invalid src256 size.");

    constexpr auto dst256 =
        "0011011100100111101001100110010011010010000110101011001001101101000110000000011101000111011111101100001001010010011111100101110011100001101011010110111101011011011010111010100111001110101100000000010110001000010111110000110100100101111111100011000111000010"sv;
    static_assert(dst256.size() == 256, "Invalid src256 size.");
}

TEST(Converter, EmptyStringToUint8Array)
{
    constexpr auto kResultArray = Converter::to_uint8_array("");
    constexpr std::array<uint8_t, 0> kExpectedArray{};
    static_assert(rabbit::is_equal_arrays(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(Converter, String_1_ToUint8Array)
{
    constexpr auto kResultArray = Converter::to_uint8_array("1");
    constexpr std::array<uint8_t, 1> kExpectedArray{0b10000000};
    static_assert(rabbit::is_equal_arrays(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(Converter, String_0_ToUint8Array)
{
    constexpr auto kResultArray = Converter::to_uint8_array("0");
    constexpr std::array<uint8_t, 1> kExpectedArray{0b00000000};
    static_assert(rabbit::is_equal_arrays(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(Converter, String_101_ToUint8Array)
{
    constexpr auto kResultArray = Converter::to_uint8_array("101");
    constexpr std::array<uint8_t, 1> kExpectedArray{0b10100000};
    static_assert(rabbit::is_equal_arrays(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(Converter, String_10100101_ToUint8Array)
{
    constexpr auto kResultArray = Converter::to_uint8_array("10100101");
    constexpr std::array<uint8_t, 1> kExpectedArray{0b10100101};
    static_assert(rabbit::is_equal_arrays(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(Converter, String_101001011_ToUint8Array)
{
    constexpr auto kResultArray = Converter::to_uint8_array("101001011");
    constexpr std::array<uint8_t, 2> kExpectedArray{0b10100101, 0b10000000};
    static_assert(rabbit::is_equal_arrays(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(Converter, String_10101010011111100100001_ToUint8Array)
{
    constexpr auto kResultArray =
        Converter::to_uint8_array("10101010011111100100001");
    constexpr std::array<uint8_t, 3> kExpectedArray{0b10101010, 0b01111110,
                                                    0b01000010};
    static_assert(rabbit::is_equal_arrays(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(Converter, String_1010101001111110110000111_ToUint8Array)
{
    constexpr auto kResultArray =
        Converter::to_uint8_array("1010101001111110110000111");
    constexpr std::array<uint8_t, 4> kExpectedArray{0b10101010, 0b01111110,
                                                    0b11000011, 0b10000000};
    static_assert(rabbit::is_equal_arrays(kResultArray, kExpectedArray),
                  "kResultArray must be equal to kExpectedArray");
}

TEST(Converter, Uint8Array_1010101001111110110000111_ToString)
{
    constexpr std::array<uint8_t, 4> kBitsArray{0b10101010, 0b01111110,
                                                0b11000011, 0b10000000};
    static constexpr auto kSymbols =
        Converter::to_symbol_array(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "10101010011111101100001110000000";
    static_assert(kResult == kExpected, "Invalid kResult.");
}

TEST(Converter, Uint8Array_1010101001111110110000111_ToString_Shorten)
{
    constexpr std::array<uint8_t, 4> kBitsArray{0b10101010, 0b01111110,
                                                0b11000011, 0b10000000};
    static constexpr auto kSymbols =
        Converter::to_symbol_array<25>(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "1010101001111110110000111";
    static_assert(kResult == kExpected, "Invalid kResult.");
}

TEST(Converter, Empty_Uint8Array_ToString)
{
    constexpr std::array<uint8_t, 0> kBitsArray{};
    static constexpr auto kSymbols =
        Converter::to_symbol_array(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "";
    static_assert(kResult == kExpected, "Invalid kResult.");
}

TEST(Converter, Uint8Array_1_ToString)
{
    constexpr std::array<uint8_t, 1> kBitsArray{0b10000000};
    static constexpr auto kSymbols =
        Converter::to_symbol_array(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "10000000";
    static_assert(kResult == kExpected, "Invalid kResult.");
}

TEST(Converter, Uint8Array_1_ToString_Shorten)
{
    constexpr std::array<uint8_t, 1> kBitsArray{0b10001100};
    static constexpr auto kSymbols =
        Converter::to_symbol_array<1>(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "1";
    static_assert(kResult == kExpected, "Invalid kResult.");
}

TEST(Converter, Uint8Array_10001100_ToString_Shorten)
{
    constexpr std::array<uint8_t, 1> kBitsArray{0b10001100};
    static constexpr auto kSymbols =
        Converter::to_symbol_array<5>(std::move(kBitsArray));
    constexpr std::string_view kResult{kSymbols.data(), kSymbols.size()};
    constexpr std::string_view kExpected = "10001";
    static_assert(kResult == kExpected, "Invalid kResult.");
}

TEST(BitStrOps, BitArrayAfterAddBits)
{
    constexpr std::size_t DstOffset = 0;
    constexpr std::size_t SrcOffset = 3;
    constexpr std::size_t NBits = 10;

    using namespace std::string_view_literals;
    constexpr auto kDst = "10101010011"sv;
    constexpr auto kSrc = "11111100100001"sv;

    constexpr auto kExcpected =
        BitStrOps::addBitsResult<DstOffset, SrcOffset, kDst.size(), kSrc.size(),
                                 NBits>(kDst, kSrc);

    auto dstBuf = Converter::to_uint8_array<kDst.size()>(kDst);
    constexpr auto kSrcBuf = Converter::to_uint8_array<kSrc.size()>(kSrc);
    constexpr rabbit::DstBitOffset kDstOffset{DstOffset};
    constexpr rabbit::SrcBitOffset kSrcOffset{SrcOffset};
    constexpr rabbit::NumBits kNBits{NBits};
    rabbit::BinOps::addBits(dstBuf.data(), kDstOffset, kSrcBuf.data(),
                            kSrcOffset, kNBits);
    ASSERT_TRUE(rabbit::is_equal_arrays(dstBuf, kExcpected));
}

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

using bin_op = rabbit::details::Operation;

TEST_F(N2BinOpsTest, ReaderConstructor)
{
    ASSERT_EQ(formatStr_.size(), 13);
    rawData_[0] = 0b00001111;
    rawData_[1] = 0b00000101;
}

TEST(BinOpsTest, Mask1)
{
    constexpr auto kMask = bin_op::mask<uint_fast8_t>(2, 3);
    constexpr uint_fast8_t expectedMask{0b00111000};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, InvertedMask1)
{
    constexpr auto kMask = bin_op::invertedMask<uint_fast8_t>(2, 3);
    constexpr uint_fast8_t expectedMask{0b11000111};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, Mask2)
{
    constexpr auto kMask = bin_op::mask<uint_fast16_t>(6, 5);
    constexpr uint_fast16_t expectedMask{0b0000001111100000};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, InvertedMask2)
{
    constexpr auto kMask = bin_op::invertedMask<uint_fast16_t>(6, 5);
    constexpr uint_fast16_t expectedMask{0b1111110000011111};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, Mask3)
{
    constexpr auto kMask = bin_op::mask<uint_fast32_t>(7, 10);
    constexpr uint_fast32_t expectedMask{0b00000001111111111000000000000000};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, InvertedMask3)
{
    constexpr auto kMask = bin_op::invertedMask<uint_fast32_t>(7, 10);
    constexpr uint_fast32_t expectedMask{0b11111110000000000111111111111111};
    ASSERT_EQ(kMask, expectedMask);
}

TEST_F(N2BinOpsTest, addValue_DstOffset_0_NBits_1_Of_16)
{
    constexpr uint16_t value = 1;
    constexpr rabbit::NumBits kNBits(1);
    constexpr rabbit::DstBitOffset kOffset(0);
    rabbit::BinOps::addValue(rawData_.data(), kOffset, value, kNBits);
    ASSERT_EQ(rawData_[0], 0b10000000);
    ASSERT_EQ(rawData_[1], 0b00000000);
}

TEST_F(N2BinOpsTest, addValue_DstOffset_0_NBits_3_Of_16)
{
    constexpr uint16_t value = 5;
    constexpr rabbit::NumBits kNBits(3);
    constexpr rabbit::DstBitOffset kOffset(0);
    rabbit::BinOps::addValue(rawData_.data(), kOffset, value, kNBits);
    ASSERT_EQ(rawData_[0], 0b10100000);
    ASSERT_EQ(rawData_[1], 0b00000000);
}

TEST_F(N2BinOpsTest, addValue_DstOffset_2_NBits_8_Of_16)
{
    rawData_[0] = 0b10000000;
    rawData_[1] = 0b00010010;

    constexpr uint16_t value = 0b11111111;
    constexpr rabbit::NumBits kNBits(8);
    constexpr rabbit::DstBitOffset kOffset(2);

    rabbit::BinOps::addValue(rawData_.data(), kOffset, value, kNBits);
    ASSERT_EQ(rawData_[0], 0b10111111);
    ASSERT_EQ(rawData_[1], 0b11010010);
}

TEST_F(N2BinOpsTest, addValue_DstOffset_1_NBits_11_Of_16)
{
    rawData_[0] = 0b10011100;
    rawData_[1] = 0b01100110;

    constexpr uint16_t value = 0b0000011101111111;
    constexpr rabbit::NumBits kNBits(11);
    constexpr rabbit::DstBitOffset kOffset(1);

    rabbit::BinOps::addValue(rawData_.data(), kOffset, value, kNBits);
    ASSERT_EQ(rawData_[0], 0b11110111);
    ASSERT_EQ(rawData_[1], 0b11110110);
}

TEST(BinOpsTest, BytePtrAtPointerToMostSignificantByte)
{
    const uint32_t value = 0;
    uint8_t const *MSBPtr = nullptr;
    auto ptr = bin_op::bytePtrAt<0>(value);
    if constexpr (rabbit::kEndian == rabbit::eEndian::kLittle)
    {
        MSBPtr = reinterpret_cast<uint8_t const *>(&value) + sizeof(value) - 1;
    }
    else if constexpr (rabbit::kEndian == rabbit::eEndian::kBig)
    {
        MSBPtr = reinterpret_cast<uint8_t const *>(&value);
    }
    ASSERT_EQ(ptr, MSBPtr);
}

TEST(BinOpsTest, BytePtrAtPointerToLeastSignificantByte)
{
    const uint32_t value = 0;
    uint8_t const *MSBPtr = nullptr;
    auto ptr = bin_op::bytePtrAt<sizeof(value) - 1>(value);
    if constexpr (rabbit::kEndian == rabbit::eEndian::kLittle)
    {
        MSBPtr = reinterpret_cast<uint8_t const *>(&value);
    }
    else if constexpr (rabbit::kEndian == rabbit::eEndian::kBig)
    {
        MSBPtr = reinterpret_cast<uint8_t const *>(&value) + sizeof(value) - 1;
    }
    ASSERT_EQ(ptr, MSBPtr);
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
constexpr decltype(auto) check_highNBits(T &&aValue,
                                         uint_fast8_t aNBits) noexcept
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
constexpr decltype(auto) check_lowNBits(T &&aValue,
                                        uint_fast8_t aNBits) noexcept
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

TEST(BinOpsTest, nBitsWithOffset)
{
    using U = uint32_t;
    constexpr U valueToAdd{0b10101010'11100101'00010000'11110111};
    constexpr U result = bin_op::nBitsWithOffset(valueToAdd, 19, 3);
    ASSERT_EQ(result, U{0b00010100'01000011'11011100'00000000});
}

TEST_F(N1BinOpsTest, addValue_DstOffset_2_NBits_4_Of_8)
{
    using U = uint8_t;
    constexpr std::size_t kMaxBytes = sizeof(U);
    using UIntT = rabbit::FastUInt<kMaxBytes>;
    constexpr UIntT valueToAdd{0b01001011};
    constexpr rabbit::NumBits kNBits(4);
    constexpr rabbit::DstBitOffset kOffset(2);
    rabbit::BinOps::addValue(rawData_.data(), kOffset, valueToAdd, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00101100});
}

TEST_F(N1BinOpsTest, addValue_DstOffset_0_NBits_5_Of_8)
{
    using U = uint8_t;
    constexpr std::size_t kMaxBytes = sizeof(U);
    using UIntT = rabbit::FastUInt<kMaxBytes>;
    UIntT valueToAdd{0b01001011};
    constexpr rabbit::NumBits kNBits(5);
    constexpr rabbit::DstBitOffset kOffset(0);
    rawData_[0] = 0b01001110;
    rabbit::BinOps::addValue(rawData_.data(), kOffset, valueToAdd, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b01011110});
}

TEST_F(N2BinOpsTest, addValue_DstOffset_5_NBits_4_Of_8)
{
    using U = uint8_t;
    constexpr std::size_t kMaxBytes = sizeof(U) + 1;
    using UIntT = rabbit::FastUInt<kMaxBytes>;
    constexpr UIntT valueToAdd{0b01001011};
    constexpr rabbit::NumBits kNBits(4);
    constexpr rabbit::DstBitOffset kOffset(5);
    rawData_[0] = 0b10111111;
    rawData_[1] = 0b11110111;
    rabbit::BinOps::addValue(rawData_.data(), kOffset, valueToAdd, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10111101});
    ASSERT_EQ(rawData_[1], uint8_t{0b11110111});
}

TEST_F(N3BinOpsTest, addValue_DstOffset_6_NBits_15_Of_16)
{
    using U = uint16_t;
    constexpr std::size_t kMaxBytes = sizeof(U) + 1;
    using UIntT = rabbit::FastUInt<kMaxBytes>;
    constexpr UIntT valueToAdd{0b01001011'00111101};
    constexpr rabbit::NumBits kNBits(15);
    constexpr rabbit::DstBitOffset kOffset(6);
    rawData_[0] = 0b10111111;
    rawData_[1] = 0b11110111;
    rawData_[2] = 0b11110101;
    rabbit::BinOps::addValue(rawData_.data(), kOffset, valueToAdd, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10111110});
    ASSERT_EQ(rawData_[1], uint8_t{0b01011001});
    ASSERT_EQ(rawData_[2], uint8_t{0b11101101});
}

TEST_F(N3BinOpsTest, addValue_DstOffset_5_NBits_16_Of_16)
{
    using U = uint16_t;
    constexpr std::size_t kMaxBytes = sizeof(U) + 1;
    using UIntT = rabbit::FastUInt<kMaxBytes>;
    constexpr UIntT valueToAdd{0b01001011'00111101};
    constexpr rabbit::NumBits kNBits(16);
    constexpr rabbit::DstBitOffset kOffset(5);
    rawData_[0] = 0b10111111;
    rawData_[1] = 0b11110111;
    rawData_[2] = 0b11110101;
    rabbit::BinOps::addValue(rawData_.data(), kOffset, valueToAdd, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10111010});
    ASSERT_EQ(rawData_[1], uint8_t{0b01011001});
    ASSERT_EQ(rawData_[2], uint8_t{0b11101101});
}

TEST_F(N8BinOpsTest, addValue_DstOffset_5_NBits_30_Of_32)
{
    using U = uint32_t;
    constexpr std::size_t kMaxBytes = sizeof(U) + 1;
    using UIntT = rabbit::FastUInt<kMaxBytes>;
    constexpr UIntT valueToAdd{0b01001011'00111101'01010101'11100011};
    constexpr rabbit::NumBits kNBits(30);
    constexpr rabbit::DstBitOffset kOffset(5);
    rawData_[0] = 0b10111111;
    rawData_[1] = 0b11110111;
    rawData_[2] = 0b11110101;
    rawData_[3] = 0b11110101;
    rawData_[4] = 0b11110101;
    rabbit::BinOps::addValue(rawData_.data(), kOffset, valueToAdd, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10111001});
    ASSERT_EQ(rawData_[1], uint8_t{0b01100111});
    ASSERT_EQ(rawData_[2], uint8_t{0b10101010});
    ASSERT_EQ(rawData_[3], uint8_t{0b10111100});
    ASSERT_EQ(rawData_[4], uint8_t{0b01110101});
    ASSERT_EQ(rawData_[5], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[6], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[7], uint8_t{0b00000000});
}

TEST_F(N8BinOpsTest, addValue_DstOffset_6_NBits_43_Of_64)
{
    using U = uint64_t;
    constexpr std::size_t kMaxBytes = sizeof(U);
    using UIntT = rabbit::FastUInt<kMaxBytes>;
    constexpr UIntT valueToAdd{
        0b01001011'00111101'01010101'11100011'11100011'11100011'11100011'11101011};
    constexpr rabbit::NumBits kNBits(43);
    constexpr rabbit::DstBitOffset kOffset(6);
    rawData_[0] = 0b10111111;
    rawData_[1] = 0b11110111;
    rawData_[2] = 0b11110101;
    rawData_[3] = 0b11110101;
    rawData_[4] = 0b11110101;
    rabbit::BinOps::addValue(rawData_.data(), kOffset, valueToAdd, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10111110});
    ASSERT_EQ(rawData_[1], uint8_t{0b11110001});
    ASSERT_EQ(rawData_[2], uint8_t{0b11110001});
    ASSERT_EQ(rawData_[3], uint8_t{0b11110001});
    ASSERT_EQ(rawData_[4], uint8_t{0b11110001});
    ASSERT_EQ(rawData_[5], uint8_t{0b11110101});
    ASSERT_EQ(rawData_[6], uint8_t{0b10000000});
    ASSERT_EQ(rawData_[7], uint8_t{0b00000000});
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

TEST_F(N2BinOpsTest, addBits_DstOffset_0_SrcOffset_0_NBits_0)
{
    std::array<uint8_t, 2> source{0b10101111, 0b10101111};
    constexpr rabbit::DstBitOffset kDstOffset{0};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{0};
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_0_SrcOffset_0_NBits_1)
{
    std::array<uint8_t, 2> source{0b10101111, 0b01111110};
    constexpr rabbit::DstBitOffset kDstOffset{0};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{1};
    rawData_[0] = 0b00110011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10110011});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_0_SrcOffset_0_NBits_2)
{
    std::array<uint8_t, 2> source{0b11101111, 0b01111110};
    constexpr rabbit::DstBitOffset kDstOffset{0};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{2};
    rawData_[0] = 0b00110011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b11110011});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_0_SrcOffset_0_NBits_5)
{
    std::array<uint8_t, 2> source{0b11101111, 0b01111110};
    constexpr rabbit::DstBitOffset kDstOffset{0};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{5};
    rawData_[0] = 0b00110011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b11101011});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_0_SrcOffset_0_NBits_8)
{
    std::array<uint8_t, 2> source{0b10011001, 0b01111110};
    constexpr rabbit::DstBitOffset kDstOffset{0};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{CHAR_BIT};
    rawData_[0] = 0b00110011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10011001});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_0_SrcOffset_0_NBits_9)
{
    std::array<uint8_t, 2> source{0b10011001, 0b10101010};
    constexpr rabbit::DstBitOffset kDstOffset{0};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{9};
    rawData_[0] = 0b00110011;
    rawData_[1] = 0b00110011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10011001});
    ASSERT_EQ(rawData_[1], uint8_t{0b10110011});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_0_SrcOffset_0_NBits_15)
{
    std::array<uint8_t, 2> source{0b10011001, 0b10101010};
    constexpr rabbit::DstBitOffset kDstOffset{0};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{15};
    rawData_[0] = 0b00110011;
    rawData_[1] = 0b00110011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10011001});
    ASSERT_EQ(rawData_[1], uint8_t{0b10101011});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_0_SrcOffset_0_NBits_16)
{
    std::array<uint8_t, 2> source{0b10011001, 0b10101010};
    constexpr rabbit::DstBitOffset kDstOffset{0};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{16};
    rawData_[0] = 0b00110011;
    rawData_[1] = 0b00110011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10011001});
    ASSERT_EQ(rawData_[1], uint8_t{0b10101010});
}

TEST(BinOpsTest, highNBitsWith2Offset)
{
    using U = uint_fast16_t;
    constexpr U kValue = 0b10011011'10101010;
    constexpr uint_fast8_t kDstOffset = 2;
    constexpr uint_fast8_t kNBits = 4;
    constexpr auto high4Bits =
        bin_op::highNBitsWithOffset(kValue, kNBits, kDstOffset);
    static_assert(std::is_same_v<std::remove_const_t<decltype(high4Bits)>, U>,
                  "high4Bits is of invalid type");
    ASSERT_EQ(high4Bits, U{0b00100100'00000000});
}

TEST(BinOpsTest, highNBitsWith0Offset)
{
    using U = uint_fast16_t;
    constexpr U kValue = 0b10011011'10101010;
    constexpr uint_fast8_t kDstOffset = 0;
    constexpr uint_fast8_t kNBits = 4;
    constexpr auto high4Bits =
        bin_op::highNBitsWithOffset(kValue, kNBits, kDstOffset);
    static_assert(std::is_same_v<std::remove_const_t<decltype(high4Bits)>, U>,
                  "high4Bits is of invalid type");
    ASSERT_EQ(high4Bits, U{0b10010000'00000000});
}

TEST(BinOpsTest, highNBitsWith10Offset)
{
    using U = uint_fast16_t;
    constexpr U kValue = 0b10011011'10101010;
    constexpr uint_fast8_t kDstOffset = 10;
    constexpr uint_fast8_t kNBits = 4;
    constexpr auto high4Bits =
        bin_op::highNBitsWithOffset(kValue, kNBits, kDstOffset);
    static_assert(std::is_same_v<std::remove_const_t<decltype(high4Bits)>, U>,
                  "high4Bits is of invalid type");
    ASSERT_EQ(high4Bits, U{0b00000000'00100100});
}

TEST(BinOpsTest, highNBitsWith12Offset)
{
    using U = uint_fast16_t;
    constexpr U kValue = 0b10011011'10101010;
    constexpr uint_fast8_t kDstOffset = 12;
    constexpr uint_fast8_t kNBits = 4;
    constexpr auto high4Bits =
        bin_op::highNBitsWithOffset(kValue, kNBits, kDstOffset);
    static_assert(std::is_same_v<std::remove_const_t<decltype(high4Bits)>, U>,
                  "high4Bits is of invalid type");
    ASSERT_EQ(high4Bits, U{0b00000000'00001001});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_1_SrcOffset_0_NBits_1)
{
    std::array<uint8_t, 2> source{0b10101111, 0b01111110};
    constexpr rabbit::DstBitOffset kDstOffset{1};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{1};
    rawData_[0] = 0b00110011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b01110011});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_1_SrcOffset_0_NBits_2)
{
    std::array<uint8_t, 2> source{0b10101111, 0b01111110};
    constexpr rabbit::DstBitOffset kDstOffset{1};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{2};
    rawData_[0] = 0b00110011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b01010011});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_1_SrcOffset_0_NBits_7)
{
    std::array<uint8_t, 2> source{0b10101101, 0b01111110};
    constexpr rabbit::DstBitOffset kDstOffset{1};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{7};
    rawData_[0] = 0b00110011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b01010110});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_1_SrcOffset_0_NBits_0)
{
    std::array<uint8_t, 2> source{0b10101101, 0b01111110};
    constexpr rabbit::DstBitOffset kDstOffset{1};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{0};
    rawData_[0] = 0b00110011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00110011});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_1_SrcOffset_0_NBits_8)
{
    std::array<uint8_t, 2> source{0b10101101, 0b01111110};
    constexpr rabbit::DstBitOffset kDstOffset{1};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{8};
    rawData_[0] = 0b00110011;
    rawData_[1] = 0b00100011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b01010110});
    ASSERT_EQ(rawData_[1], uint8_t{0b10100011});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_3_SrcOffset_0_NBits_10)
{
    std::array<uint8_t, 2> source{0b10101101, 0b01111110};
    constexpr rabbit::DstBitOffset kDstOffset{3};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{10};
    rawData_[0] = 0b00110011;
    rawData_[1] = 0b00100011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00110101});
    ASSERT_EQ(rawData_[1], uint8_t{0b10101011});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_3_SrcOffset_0_NBits_13)
{
    std::array<uint8_t, 2> source{0b10101101, 0b01111110};
    constexpr rabbit::DstBitOffset kDstOffset{3};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{13};
    rawData_[0] = 0b00110011;
    rawData_[1] = 0b00100010;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00110101});
    ASSERT_EQ(rawData_[1], uint8_t{0b10101111});
}

TEST_F(N3BinOpsTest, addBits_DstOffset_5_SrcOffset_0_NBits_13)
{
    std::array<uint8_t, 2> source{0b10101101, 0b01111110};
    constexpr rabbit::DstBitOffset kDstOffset{5};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{13};
    rawData_[0] = 0b11011011;
    rawData_[1] = 0b00100010;
    rawData_[2] = 0b00100011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b11011101});
    ASSERT_EQ(rawData_[1], uint8_t{0b01101011});
    ASSERT_EQ(rawData_[2], uint8_t{0b11100011});
}

TEST_F(N4BinOpsTest, addBits_DstOffset_7_SrcOffset_0_NBits_23)
{
    std::array<uint8_t, 4> source{0b00001111, 0b00110011, 0b10101010,
                                  0b01111110};
    constexpr rabbit::DstBitOffset kDstOffset{7};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{23};
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101110});
    ASSERT_EQ(rawData_[1], uint8_t{0b00011110});
    ASSERT_EQ(rawData_[2], uint8_t{0b01100111});
    ASSERT_EQ(rawData_[3], uint8_t{0b01010100});
}

TEST_F(N8BinOpsTest, addBits_DstOffset_5_SrcOffset_0_NBits_35)
{
    std::array<uint8_t, 8> source{0b10101111, 0b00010111, 0b00001111,
                                  0b00110011, 0b10101010, 0b01111110,
                                  0b10000001, 0b10011001};
    constexpr rabbit::DstBitOffset kDstOffset{5};
    constexpr rabbit::SrcBitOffset kSrcOffset{0};
    constexpr rabbit::NumBits kNBits{35};
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    rawData_[4] = 0b11000011;
    rawData_[5] = 0b11100111;
    rawData_[6] = 0b10111101;
    rawData_[7] = 0b00111100;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101101});
    ASSERT_EQ(rawData_[1], uint8_t{0b01111000});
    ASSERT_EQ(rawData_[2], uint8_t{0b10111000});
    ASSERT_EQ(rawData_[3], uint8_t{0b01111001});
    ASSERT_EQ(rawData_[4], uint8_t{0b10011101});
    ASSERT_EQ(rawData_[5], uint8_t{0b11100111});
    ASSERT_EQ(rawData_[6], uint8_t{0b10111101});
    ASSERT_EQ(rawData_[7], uint8_t{0b00111100});
}

TEST_F(N1BinOpsTest, addBits_DstOffset_2_SrcOffset_5_NBits_3)
{
    std::array<uint8_t, 1> source{0b10101101};
    constexpr rabbit::DstBitOffset kDstOffset{2};
    constexpr rabbit::SrcBitOffset kSrcOffset{5};
    constexpr rabbit::NumBits kNBits{3};
    rawData_[0] = 0b10110011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101011});
}

TEST_F(N1BinOpsTest, addBits_DstOffset_3_SrcOffset_1_NBits_4)
{
    std::array<uint8_t, 1> source{0b01101000};
    constexpr rabbit::DstBitOffset kDstOffset{3};
    constexpr rabbit::SrcBitOffset kSrcOffset{1};
    constexpr rabbit::NumBits kNBits{4};
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00011010});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_4_SrcOffset_2_NBits_5)
{
    std::array<uint8_t, 1> source{0b01100110};
    constexpr rabbit::DstBitOffset kDstOffset{4};
    constexpr rabbit::SrcBitOffset kSrcOffset{2};
    constexpr rabbit::NumBits kNBits{5};
    rawData_[0] = 0b00110011;
    rawData_[1] = 0b00100010;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00111001});
    ASSERT_EQ(rawData_[1], uint8_t{0b10100010});
}

TEST_F(N2BinOpsTest, addBits_DstOffset_5_SrcOffset_2_NBits_10)
{
    std::array<uint8_t, 3> source{0b01100110, 0b10011001};
    constexpr rabbit::DstBitOffset kDstOffset{5};
    constexpr rabbit::SrcBitOffset kSrcOffset{2};
    constexpr rabbit::NumBits kNBits{10};
    rawData_[0] = 0b00111100;
    rawData_[1] = 0b11000011;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00111100});
    ASSERT_EQ(rawData_[1], uint8_t{0b11010011});
}

TEST_F(N4BinOpsTest, GetUInt16From1Byte)
{
    constexpr std::size_t kNBytes = 1;
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    const auto value = bin_op::get<uint16_t>(rawData_.data(), kNBytes);
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
    const auto value = bin_op::get<uint16_t>(rawData_.data(), kNBytes);
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
    const auto value = bin_op::get<uint32_t>(rawData_.data(), kNBytes);
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
    const auto value = bin_op::get<uint32_t>(rawData_.data(), kNBytes);
    static_assert(
        std::is_same_v<std::remove_const_t<decltype(value)>, uint32_t>,
        "value is of invalid type.");
    ASSERT_EQ(value, uint32_t{0b10101111'10000001'10011001'00111100});
}

TEST_F(N8BinOpsTest, addBits_DstOffset_5_SrcOffset_7_NBits_55)
{
    std::array<uint8_t, 8> source{0b10101111, 0b00010111, 0b00001111,
                                  0b00110011, 0b10101010, 0b01111110,
                                  0b10000001, 0b10011001};
    constexpr rabbit::DstBitOffset kDstOffset{5};
    constexpr rabbit::SrcBitOffset kSrcOffset{7};
    constexpr rabbit::NumBits kNBits{55};
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    rawData_[4] = 0b11000011;
    rawData_[5] = 0b11100111;
    rawData_[6] = 0b10111101;
    rawData_[7] = 0b00111100;
    rabbit::BinOps::addBits(rawData_.data(), kDstOffset, source.data(),
                            kSrcOffset, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101100});
    ASSERT_EQ(rawData_[1], uint8_t{0b01011100});
    ASSERT_EQ(rawData_[2], uint8_t{0b00111100});
    ASSERT_EQ(rawData_[3], uint8_t{0b11001110});
    ASSERT_EQ(rawData_[4], uint8_t{0b10101001});
    ASSERT_EQ(rawData_[5], uint8_t{0b11111010});
    ASSERT_EQ(rawData_[6], uint8_t{0b00000110});
    ASSERT_EQ(rawData_[7], uint8_t{0b01101100});
}

TEST_F(N32BinOpsTest, addValue_DstOffset_5_NBits_64_Of_64)
{
    constexpr uint64_t kValue =
        0b10101111'00010111'00001111'00110011'10101010'01111110'10000001'10011001;
    constexpr rabbit::DstBitOffset kOffset(5);
    constexpr rabbit::NumBits kNBits(64);
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    rawData_[4] = 0b11000011;
    rawData_[5] = 0b11100111;
    rawData_[6] = 0b10111101;
    rawData_[7] = 0b00111100;
    rawData_[8] = 0b11111101;
    rabbit::BinOps::addValue(rawData_.data(), kOffset, kValue, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101101});
    ASSERT_EQ(rawData_[1], uint8_t{0b01111000});
    ASSERT_EQ(rawData_[2], uint8_t{0b10111000});
    ASSERT_EQ(rawData_[3], uint8_t{0b01111001});
    ASSERT_EQ(rawData_[4], uint8_t{0b10011101});
    ASSERT_EQ(rawData_[5], uint8_t{0b01010011});
    ASSERT_EQ(rawData_[6], uint8_t{0b11110100});
    ASSERT_EQ(rawData_[7], uint8_t{0b00001100});
    ASSERT_EQ(rawData_[8], uint8_t{0b11001101});
    ASSERT_EQ(rawData_[9], uint8_t{0b00000000});
}

TEST_F(N32BinOpsTest, addValue_DstOffset_7_NBits_60_Of_64)
{
    constexpr uint64_t kValue =
        0b10101111'00010111'00001111'00110011'10101010'01111110'10000001'10011001;
    constexpr rabbit::DstBitOffset kOffset(7);
    constexpr rabbit::NumBits kNBits(60);
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    rawData_[4] = 0b11000011;
    rawData_[5] = 0b11100111;
    rawData_[6] = 0b10111101;
    rawData_[7] = 0b00111100;
    rawData_[8] = 0b11111101;
    rabbit::BinOps::addValue(rawData_.data(), kOffset, kValue, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101111});
    ASSERT_EQ(rawData_[1], uint8_t{0b11100010});
    ASSERT_EQ(rawData_[2], uint8_t{0b11100001});
    ASSERT_EQ(rawData_[3], uint8_t{0b11100110});
    ASSERT_EQ(rawData_[4], uint8_t{0b01110101});
    ASSERT_EQ(rawData_[5], uint8_t{0b01001111});
    ASSERT_EQ(rawData_[6], uint8_t{0b11010000});
    ASSERT_EQ(rawData_[7], uint8_t{0b00110011});
    ASSERT_EQ(rawData_[8], uint8_t{0b00111101});
    ASSERT_EQ(rawData_[9], uint8_t{0b00000000});
}

TEST_F(N4BinOpsTest, addValue_DstOffset_5_NBits_10_Of_32)
{
    constexpr uint32_t kValue = 0b00110011'10101010'01111110'10000001;
    constexpr rabbit::DstBitOffset kOffset(5);
    constexpr rabbit::NumBits kNBits(10);
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    rabbit::BinOps::addValue(rawData_.data(), kOffset, std::move(kValue),
                             kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101101});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000011});
    ASSERT_EQ(rawData_[2], uint8_t{0b10011001});
    ASSERT_EQ(rawData_[3], uint8_t{0b00111100});
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

TEST_F(N32BinOpsTest, addValue_DstOffset_5_NBits_63_Of_64)
{
    constexpr uint64_t kValue =
        0b11101111'00010111'00001111'00110011'10101010'01111110'10000001'10011001;
    constexpr rabbit::DstBitOffset kOffset(5);
    constexpr rabbit::NumBits kNBits(63);
    rawData_[0] = 0b10101010;
    rawData_[8] = 0b11001001;

    rabbit::BinOps::addValue(rawData_.data(), kOffset, kValue, kNBits);

    ASSERT_EQ(rawData_[0], uint8_t{0b10101110});
    ASSERT_EQ(rawData_[1], uint8_t{0b11110001});
    ASSERT_EQ(rawData_[2], uint8_t{0b01110000});
    ASSERT_EQ(rawData_[3], uint8_t{0b11110011});
    ASSERT_EQ(rawData_[4], uint8_t{0b00111010});
    ASSERT_EQ(rawData_[5], uint8_t{0b10100111});
    ASSERT_EQ(rawData_[6], uint8_t{0b11101000});
    ASSERT_EQ(rawData_[7], uint8_t{0b00011001});
    ASSERT_EQ(rawData_[8], uint8_t{0b10011001});
}

TEST_F(N32BinOpsTest, addValue_DstOffset_3_NBits_3_Of_64)
{
    constexpr uint64_t kValue =
        0b11101111'00010111'00001111'00110011'10101010'01111110'10000001'10011001;
    constexpr rabbit::DstBitOffset kOffset(3);
    constexpr rabbit::NumBits kNBits(3);
    rawData_[0] = 0b10101010;
    rawData_[8] = 0b11001001;

    rabbit::BinOps::addValue(rawData_.data(), kOffset, kValue, kNBits);

    ASSERT_EQ(rawData_[0], uint8_t{0b10100110});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[2], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[3], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[4], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[5], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[6], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[7], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[8], uint8_t{0b11001001});
}

TEST_F(N32BinOpsTest, addValue_DstOffset_2_NBits_10_Of_64)
{
    constexpr uint64_t kValue =
        0b11101111'00010111'00001111'00110011'10101010'01111110'10000001'10011001;
    constexpr rabbit::DstBitOffset kOffset(2);
    constexpr rabbit::NumBits kNBits(10);
    rawData_[0] = 0b10101010;
    rawData_[8] = 0b11001001;

    rabbit::BinOps::addValue(rawData_.data(), kOffset, kValue, kNBits);

    ASSERT_EQ(rawData_[0], uint8_t{0b10011001});
    ASSERT_EQ(rawData_[1], uint8_t{0b10010000});
    ASSERT_EQ(rawData_[2], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[3], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[4], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[5], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[6], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[7], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[8], uint8_t{0b11001001});
}

TEST_F(N32BinOpsTest, addValue_NBits_10_Of_64)
{
    constexpr uint64_t kValue =
        0b11101111'00010111'00001111'00110011'10101010'01111110'10000001'10011001;
    constexpr rabbit::NumBits kNBits(10);
    rawData_[0] = 0b10101010;
    rawData_[1] = 0b11001001;

    rabbit::BinOps::addValue(rawData_.data(), kValue, kNBits);

    ASSERT_EQ(rawData_[0], uint8_t{0b01100110});
    ASSERT_EQ(rawData_[1], uint8_t{0b01001001});
    ASSERT_EQ(rawData_[2], uint8_t{0b00000000});
}

TEST_F(N4BinOpsTest, addValue_Of_16)
{
    constexpr uint16_t kValue = 0b10000001'10011001;
    rawData_[0] = 0b10101010;
    rawData_[1] = 0b11001001;

    rabbit::BinOps::addValue(rawData_.data(), kValue);

    ASSERT_EQ(rawData_[0], uint8_t{0b10000001});
    ASSERT_EQ(rawData_[1], uint8_t{0b10011001});
    ASSERT_EQ(rawData_[2], uint8_t{0b00000000});
}

TEST_F(N8BinOpsTest, addValue_DstOffset_3_NBits_3_Of_32)
{
    constexpr uint32_t kValue = 0b10101010'01111110'10000001'10011001;
    constexpr rabbit::DstBitOffset kOffset(3);
    constexpr rabbit::NumBits kNBits(3);
    rawData_[0] = 0b10101010;
    rawData_[7] = 0b11001001;

    rabbit::BinOps::addValue(rawData_.data(), kOffset, kValue, kNBits);

    ASSERT_EQ(rawData_[0], uint8_t{0b10100110});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[2], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[3], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[4], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[5], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[6], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[7], uint8_t{0b11001001});
}

TEST_F(N8BinOpsTest, addValue_DstOffset_2_NBits_10_Of_32)
{
    constexpr uint32_t kValue = 0b10101010'01111110'10000001'10011001;
    constexpr rabbit::DstBitOffset kOffset(2);
    constexpr rabbit::NumBits kNBits(10);
    rawData_[0] = 0b10101010;
    rawData_[7] = 0b11001001;

    rabbit::BinOps::addValue(rawData_.data(), kOffset, kValue, kNBits);

    ASSERT_EQ(rawData_[0], uint8_t{0b10011001});
    ASSERT_EQ(rawData_[1], uint8_t{0b10010000});
    ASSERT_EQ(rawData_[2], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[3], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[4], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[5], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[6], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[7], uint8_t{0b11001001});
}