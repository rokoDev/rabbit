#ifndef rabbit_test_helpers_h
#define rabbit_test_helpers_h

#include <algorithm>
#include <cstddef>
#include <limits>
#include <random>
#include <string>

#include "rabbit/bin_ops.h"
#include "rabbit/details.h"
#include "rabbit/user_literals.h"

namespace rabbit
{
class test_helpers
{
   public:
    using DstBitOffset = rabbit::DstBitOffset;
    using SrcBitOffset = rabbit::SrcBitOffset;
    using NumBits = rabbit::NumBits;
    using BitOffset = rabbit::BitOffset;

    static std::string random_bit_sequence(const std::size_t aNBits);
    template <typename T>
    static T random_value()
    {
        static_assert(std::is_arithmetic_v<T>, "T must be arithmetic type.");
        std::random_device rd;
        std::mt19937 mt(rd());
        if constexpr (std::is_integral_v<T>)
        {
            std::uniform_int_distribution<T> dist(
                std::numeric_limits<T>::min(), std::numeric_limits<T>::max());
            return dist(mt);
        }
        else
        {
            std::uniform_real_distribution<T> dist(
                std::numeric_limits<T>::min(), std::numeric_limits<T>::max());
            return dist(mt);
        }
    }

    static constexpr uint8_t bitAtIndex(std::size_t aPos) noexcept
    {
        return static_cast<uint8_t>(1_u8 << (CHAR_BIT - 1 - aPos % CHAR_BIT));
    }

    static constexpr void setBit(uint8_t *const aData,
                                 std::size_t aPos) noexcept
    {
        aData[aPos / CHAR_BIT] |= bitAtIndex(aPos);
    }

    static constexpr void resetBit(uint8_t *const aData,
                                   std::size_t aPos) noexcept
    {
        aData[aPos / CHAR_BIT] &= ~bitAtIndex(aPos);
    }

    static constexpr bool isBitSet(uint8_t const *const aData,
                                   std::size_t aPos) noexcept
    {
        return aData[aPos / CHAR_BIT] & bitAtIndex(aPos);
    }

    static constexpr void to_uint8_buf(uint8_t *const aDst,
                                       const std::size_t aDstOffset,
                                       std::string_view aBitStr) noexcept
    {
        for (std::size_t i = 0; i < aBitStr.size(); ++i)
        {
            if (aBitStr[i] == '0')
            {
                resetBit(aDst, aDstOffset + i);
            }
            else
            {
                setBit(aDst, aDstOffset + i);
            }
        }
    }

    static constexpr void to_uint8_buf(uint8_t *const aDst,
                                       std::string_view aBitStr) noexcept
    {
        to_uint8_buf(aDst, 0, aBitStr);
    }

    template <std::size_t NBits>
    static constexpr auto to_uint8_array(std::string_view aBitStr) noexcept
    {
        std::array<uint8_t, rabbit::details::bytesCount(NBits)> result{};
        to_uint8_buf(result.data(), aBitStr);
        return result;
    }

    static constexpr void to_symbol_buf(char *const aDst,
                                        uint8_t const *const aSrc,
                                        const std::size_t aNBits) noexcept
    {
        for (std::size_t i = 0; i < aNBits; ++i)
        {
            aDst[i] = isBitSet(aSrc, i) ? '1' : '0';
        }
    }

    template <std::size_t NBits, typename ArrayT>
    static constexpr decltype(auto) to_symbol_array(ArrayT &&aArray) noexcept
    {
        using namespace rabbit;
        static_assert(std::is_same_v<std_array_data_t<ArrayT>, uint8_t>,
                      "Elements of aArray must belong to uint8_t type.");
        constexpr std::size_t kMaxNBytes = std_array_size_v<ArrayT>;
        static_assert(NBits <= kMaxNBytes * CHAR_BIT, "Invalid NBits.");
        std::array<char, NBits> result{};
        to_symbol_buf(result.data(), aArray.data(), NBits);
        return result;
    }

    template <typename ArrayT>
    static constexpr decltype(auto) to_symbol_array(ArrayT &&aArray) noexcept
    {
        constexpr std::size_t kNBytes = rabbit::std_array_size_v<ArrayT>;
        return to_symbol_array<kNBytes * CHAR_BIT>(
            std::forward<ArrayT>(aArray));
    }

    class BitAccumulator
    {
       public:
        BitAccumulator() = delete;
        constexpr BitAccumulator(uint8_t *const aData,
                                 std::size_t aStartIndex) noexcept
            : data_(aData), bitIndex_(aStartIndex)
        {
        }

        constexpr BitAccumulator(uint8_t *const aData) noexcept
            : BitAccumulator(aData, 0)
        {
        }

        constexpr void add(std::string_view aBitStr) noexcept
        {
            to_uint8_buf(data_, bitIndex_, aBitStr);
            bitIndex_ += aBitStr.size();
        }

       private:
        uint8_t *const data_{};
        std::size_t bitIndex_{};
    };

    static constexpr void copyBitsExpected(uint8_t *const aResult,
                                           std::string_view aDst,
                                           const DstBitOffset aDstOffset,
                                           std::string_view aSrc,
                                           const SrcBitOffset aSrcOffset,
                                           const NumBits aNBits) noexcept
    {
        assert(aDstOffset.get() + aNBits.get() <= aDst.size() &&
               "Invalid arguments.");
        assert(aSrcOffset.get() + aNBits.get() <= aSrc.size() &&
               "Invalid arguments.");

        BitAccumulator accumulator(aResult);

        auto prefix = aDst.substr(0, aDstOffset.get());
        accumulator.add(prefix);

        auto content = aSrc.substr(aSrcOffset.get(), aNBits.get());
        accumulator.add(content);

        auto suffix = aDst.substr(aDstOffset.get() + aNBits.get());
        accumulator.add(suffix);
    }

    template <typename DataT>
    static auto addValueExpected(const DataT &aData) noexcept
    {
        const auto [aDstBitStr, aValue, aOffset, aNBits] = aData;
        using T = std::decay_t<decltype(aValue)>;
        static_assert(rabbit::is_uint_v<T>, "Invalid T");
        constexpr auto kValueBitSize = rabbit::utils::num_bits<T>();
        assert(aNBits.get() <= kValueBitSize && "Invalid aNBits");
        assert(aOffset.get() + aNBits.get() <= aDstBitStr.size());

        std::vector<char> etalonBitArr(aDstBitStr.cbegin(), aDstBitStr.cend());

        const auto kValueBitsArray =
            to_symbol_array(rabbit::details::to_uint8_array(aValue));
        std::string_view valueBitStr{kValueBitsArray.data(), kValueBitSize};

        std::string_view bitsStrToAdd =
            valueBitStr.substr(valueBitStr.size() - aNBits.get());

        auto posToPaste = etalonBitArr.begin();
        std::advance(posToPaste, aOffset.get());

        std::copy_n(bitsStrToAdd.cbegin(), aNBits.get(), posToPaste);

        std::vector<uint8_t> result(
            rabbit::details::bytesCount(etalonBitArr.size()));
        std::string_view resultStr{etalonBitArr.data(), etalonBitArr.size()};
        to_uint8_buf(result.data(), resultStr);
        return result;
    }

    template <typename T>
    constexpr static T getValueExpected(std::string_view aSrcBitStr,
                                        SrcBitOffset aSrcOffset,
                                        NumBits aNBits) noexcept
    {
        static_assert(rabbit::is_uint_v<T>, "Invalid T");
        constexpr auto kBitsInValue = rabbit::utils::num_bits<T>();
        assert(aNBits.get() <= kBitsInValue);
        assert(aSrcOffset.get() <= CHAR_BIT);
        assert(aSrcOffset.get() + aNBits.get() <= aSrcBitStr.size());

        auto valueBitsArr = make_array<char, kBitsInValue>('0');
        std::string_view valueBitStr{aSrcBitStr.data() + aSrcOffset.get(),
                                     aNBits.get()};

        for (std::size_t i = 0; i < valueBitStr.size(); ++i)
        {
            valueBitsArr[kBitsInValue - aNBits.get() + i] = valueBitStr[i];
        }
        std::string_view allValueBitsStr{valueBitsArr.data(),
                                         valueBitsArr.size()};

        std::array<uint8_t, sizeof(T)> resultByteArray =
            to_uint8_array<kBitsInValue>(allValueBitsStr);
        T result = details::uint8_buf_to_value<T>(resultByteArray.data());
        return result;
    }
};
}  // namespace rabbit

#endif /* rabbit_test_helpers_h */