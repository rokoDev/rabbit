#ifndef add_bits_tests_h
#define add_bits_tests_h

#include <gtest/gtest.h>

#include <string_view>
#include <tuple>

#include "buffer_tests.h"
#include "rabbit/bin_ops.h"
#include "test_helpers.h"

namespace rabbit
{
namespace test
{
using ::testing::TestWithParam;

using namespace std::string_view_literals;
using DstBitOffset = rabbit::DstBitOffset;
using SrcBitOffset = rabbit::SrcBitOffset;
using NumBits = rabbit::NumBits;
using BitOffset = rabbit::BitOffset;
using DataCount =
    strong::strong_type<struct DataCountTag, std::size_t, rabbit::NecessaryOps>;
using helpers = rabbit::test_helpers;

class TestData
{
   public:
    using RawBits = std::vector<uint8_t>;
    TestData() = delete;
    TestData(const TestData&) = delete;
    const TestData& operator=(const TestData&) = delete;

    template <typename DstT, typename SrcT, typename NameT>
    TestData(DstT&& aDst, SrcT&& aSrc, NameT&& aName)
        : name_(std::forward<NameT>(aName))
        , dstBitsStr_(std::forward<DstT>(aDst))
        , srcBitsStr_(std::forward<SrcT>(aSrc))
        , srcBits_(binVectorFromBitStr(srcBitsStr_))
    {
    }

    std::string_view name() const { return name_; }

    std::string_view srcBitsStr() const { return srcBitsStr_; }

    std::string_view dstBitsStr() const { return dstBitsStr_; }

    const RawBits& srcBits() const { return srcBits_; }

    std::size_t maxBytesInSrc() const
    {
        return rabbit::details::bytesCount(srcBitsStr_.size());
    }

    std::size_t maxBytesInDst() const
    {
        return rabbit::details::bytesCount(dstBitsStr_.size());
    }

   private:
    RawBits binVectorFromBitStr(const std::string& aBitStr) const
    {
        const auto kNBytes = rabbit::details::bytesCount(aBitStr.size());
        RawBits result(kNBytes);
        helpers::to_uint8_buf(result.data(), aBitStr);
        return result;
    }

    std::string name_;
    std::string dstBitsStr_;
    std::string srcBitsStr_;
    RawBits srcBits_;
};

class TestDataFactory
{
   public:
    using TestDataVectorT = std::vector<std::shared_ptr<TestData>>;
    using NameGeneratorT = std::function<std::string(const std::size_t)>;

    static TestDataVectorT randomData(const DataCount aDataCount,
                                      const NumBits aNBits,
                                      NameGeneratorT aNameGenerator)
    {
        TestDataVectorT result;
        result.reserve(aDataCount.get());
        for (std::size_t i = 0; i < aDataCount.get(); ++i)
        {
            auto srcBitStr = helpers::random_bit_sequence(aNBits.get());
            auto dstBitStr = helpers::random_bit_sequence(aNBits.get());
            auto name = aNameGenerator ? aNameGenerator(i)
                                       : ("Random" + std::to_string(i));
            auto pData = std::make_shared<TestData>(
                std::move(dstBitStr), std::move(srcBitStr), std::move(name));
            result.push_back(std::move(pData));
        }
        return result;
    }

    static TestDataVectorT randomData(const DataCount aDataCount,
                                      const NumBits aNBits)
    {
        return randomData(aDataCount, aNBits, nullptr);
    }

    static TestDataVectorT presetData()
    {
        TestDataVectorT result;
        static constexpr std::string_view kSrc256 =
            "1101011011100000001100001110110010100101000111000001000101100111111111000011010110000101000111011110011101110010011111110010110100010010101010000101101100100111101111001010101110110011111010100111010110001101010110001101010010100100100011100110111000110001"sv;
        static constexpr std::string_view kDst256 =
            "0100011000101100101011110101001100000011011011110100111010011001010001001100100110111011110101001110011101110010101011011101011011110000000011111111100100001110100100110110011110101101000111100010111111011100110011111001111011110110100000100100111101111010"sv;
        result.push_back(
            std::make_shared<TestData>(kDst256, kSrc256, std::string("Data0")));
        return result;
    }
};

using AddBits5TestDataT =
    std::tuple<std::shared_ptr<TestData>, NumBits, DstBitOffset, SrcBitOffset>;
using AddBits4TestDataT =
    std::tuple<std::shared_ptr<TestData>, NumBits, BitOffset>;
using AddBits3TestDataT = std::tuple<std::shared_ptr<TestData>, NumBits>;
using BytesInDstSrcT = std::tuple<std::size_t, std::size_t>;

class AddBitsBase
{
   protected:
    void arrangeExpectedActual(const AddBits5TestDataT& aParam)
    {
        const auto [kData, kNBits, kDstOffset, kSrcOffset] = aParam;
        const std::size_t kDstNBytes =
            rabbit::details::bytesCount(kDstOffset.get(), kNBits.get());
        auto dstBits = kData->dstBitsStr().substr(0, kDstNBytes * CHAR_BIT);

        // expected
        expected_.assign(kDstNBytes, 0_u8);
        helpers::addBitsExpected(expected_.data(), dstBits, kDstOffset,
                                 kData->srcBitsStr(), kSrcOffset, kNBits);

        // actual
        actual_.assign(kDstNBytes, 0_u8);
        helpers::to_uint8_buf(actual_.data(), dstBits);
    }
    std::vector<uint8_t> expected_;
    std::vector<uint8_t> actual_;
};

class AddBitsWith5Args
    : public AddBitsBase
    , public TestWithParam<AddBits5TestDataT>
{
   protected:
    static BytesInDstSrcT bytesInDstSrc(const AddBits5TestDataT& aParam)
    {
        const auto [kData, kNBits, kDstOffset, kSrcOffset] = aParam;
        const std::size_t kDstNBytes =
            rabbit::details::bytesCount(kDstOffset.get(), kNBits.get());
        const std::size_t kSrcNBytes =
            rabbit::details::bytesCount(kSrcOffset.get(), kNBits.get());
        return {kDstNBytes, kSrcNBytes};
    }
};

class AddBitsWith4Args
    : public AddBitsBase
    , public TestWithParam<AddBits4TestDataT>
{
   protected:
    static std::size_t bytesInDstSrc(const AddBits4TestDataT& aParam)
    {
        auto [kData, kNBits, kOffset] = aParam;
        return rabbit::details::bytesCount(kOffset.get(), kNBits.get());
    }
};

class AddBitsWith3Args
    : public AddBitsBase
    , public TestWithParam<AddBits3TestDataT>
{
   protected:
    static std::size_t bytesInDstSrc(const AddBits3TestDataT& aParam)
    {
        auto [kData, kNBits] = aParam;
        return rabbit::details::bytesCount(kNBits.get());
    }
};
}  // namespace test
}  // namespace rabbit

#endif /* add_bits_tests_h */