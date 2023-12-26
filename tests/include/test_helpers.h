#ifndef rabbit_test_helpers_h
#define rabbit_test_helpers_h

#include <endian/endian.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include <algorithm>
#include <cstddef>
#include <cstdlib>
#include <limits>
#include <random>
#include <string>
#include <tuple>

#include "rabbit/bin_ops.h"
#include "rabbit/details.h"

namespace test_utils
{
std::string random_bit_sequence(const std::size_t aNBits);
template <typename T>
T random_value()
{
    static_assert(std::is_arithmetic_v<T>, "T must be arithmetic type.");
    std::random_device rd;
    std::mt19937 mt(rd());
    if constexpr (std::is_integral_v<T>)
    {
        std::uniform_int_distribution<T> dist(std::numeric_limits<T>::min(),
                                              std::numeric_limits<T>::max());
        return dist(mt);
    }
    else
    {
        std::uniform_real_distribution<T> dist(std::numeric_limits<T>::min(),
                                               std::numeric_limits<T>::max());
        return dist(mt);
    }
}

template <typename CoreOpsT>
class bits;

template <>
class bits<::rabbit::v1::Core>
{
   public:
    using core = ::rabbit::v1::Core;
    using DstOffset = ::rabbit::DstOffset;
    using SrcOffset = ::rabbit::SrcOffset;
    using NumBits = ::rabbit::NumBits;
    using Offset = ::rabbit::Offset;
    using BitIndex = ::rabbit::BitIndex;
    using Dst = ::rabbit::Dst;
    using Src = ::rabbit::Src;
    using n_bytes = ::rabbit::n_bytes;

    struct bb_index_t
    {
        std::size_t byte{};
        std::uint8_t bit{};

        bb_index_t(const bb_index_t &) = default;
        bb_index_t &operator=(const bb_index_t &) = default;
        bb_index_t(bb_index_t &&) = default;
        bb_index_t &operator=(bb_index_t &&) = default;

        template <typename BytePos, typename BitPos>
        inline constexpr bb_index_t(BytePos &&aByte, BitPos &&aBit) noexcept
            : byte{static_cast<std::size_t>(aByte)}
            , bit{static_cast<std::uint8_t>(aBit)}
        {
            if (!__builtin_is_constant_evaluated())
            {
                assert(aBit < CHAR_BIT);
            }
        }

        inline constexpr bb_index_t(BitIndex aBitIndex) noexcept
            : bb_index_t(
                  [aBitIndex]()
                  {
                      constexpr auto kCharBit =
                          static_cast<BitIndex::value_type>(CHAR_BIT);
                      if (__builtin_is_constant_evaluated())
                      {
                          return bb_index_t{
                              aBitIndex / kCharBit,
                              kCharBit - 1 - aBitIndex % kCharBit};
                      }
                      else
                      {
                          const auto divided =
                              std::div(static_cast<int>(aBitIndex), CHAR_BIT);
                          return bb_index_t{divided.quot,
                                            CHAR_BIT - 1 - divided.rem};
                      }
                  }())
        {
        }
    };

    static inline constexpr std::byte bitAtIndex(std::size_t aBitPos) noexcept
    {
        return std::byte{1} << aBitPos;
    }

    static constexpr void setBit(Dst aData, std::size_t aPos) noexcept
    {
        auto [byte, bit] = bb_index_t(BitIndex{aPos});
        aData[byte] |= bitAtIndex(bit);
    }

    static constexpr void resetBit(Dst aData, std::size_t aPos) noexcept
    {
        auto [byte, bit] = bb_index_t(BitIndex{aPos});
        aData[byte] &= ~bitAtIndex(bit);
    }

    static constexpr bool isBitSet(Src aData, std::size_t aPos) noexcept
    {
        auto [byte, bit] = bb_index_t(BitIndex{aPos});
        return (aData[byte] & bitAtIndex(bit)) != std::byte{0};
    }

    static constexpr void to_byte_buf(Dst aDst, const std::size_t aDstOffset,
                                      std::string_view aBitStr) noexcept
    {
        const auto kNumBits = aBitStr.size();
        for (std::size_t i = 0; i < kNumBits; ++i)
        {
            const auto index = i;
            if (aBitStr[index] == '0')
            {
                resetBit(aDst, aDstOffset + i);
            }
            else
            {
                setBit(aDst, aDstOffset + i);
            }
        }
    }

    static constexpr void to_byte_buf(Dst aDst,
                                      std::string_view aBitStr) noexcept
    {
        to_byte_buf(aDst, 0, aBitStr);
    }

    template <std::size_t NBits>
    static constexpr auto to_byte_array(const std::string_view aBitStr) noexcept
    {
        std::array<std::byte, rabbit::details::bytes_count(NumBits{NBits})>
            result{};
        to_byte_buf(Dst{result.data()}, aBitStr);
        return result;
    }

    static constexpr void to_symbol_buf(char *const aDst, Src aSrc,
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
        static_assert(
            std::is_same_v<utils::std_array_data_t<ArrayT>, std::byte>,
            "Elements of aArray must belong to std::uint8_t type.");
        constexpr std::size_t kMaxNBytes = utils::std_array_size_v<ArrayT>;
        static_assert(NBits <= kMaxNBytes * CHAR_BIT, "Invalid NBits.");
        std::array<char, NBits> result{};
        to_symbol_buf(result.data(), Src{aArray.data()}, NBits);
        return result;
    }

    template <typename ArrayT>
    static constexpr decltype(auto) to_symbol_array(ArrayT &&aArray) noexcept
    {
        constexpr std::size_t kNBytes = utils::std_array_size_v<ArrayT>;
        return to_symbol_array<kNBytes * CHAR_BIT>(
            std::forward<ArrayT>(aArray));
    }

    static constexpr void copy_bits_expected_str(
        char *const aDst, [[maybe_unused]] std::size_t aDstSize,
        const DstOffset aDstOffset, std::string_view aSrc,
        const SrcOffset aSrcOffset, const NumBits aNBits) noexcept
    {
        assert(aDst != nullptr);
        assert(aDstOffset < CHAR_BIT);
        assert(aSrcOffset < CHAR_BIT);
        assert(aDstOffset.get() + aNBits.get() <= aDstSize);
        assert(aSrcOffset.get() + aNBits.get() <= aSrc.size());

        if (!aNBits)
        {
            return;
        }

        char *dstBegin = aDst + aDstOffset;
        char const *srcBegin = aSrc.data() + aSrcOffset;

        if (__builtin_is_constant_evaluated())
        {
            for (std::size_t i = 0; i < aNBits; ++i)
            {
                *(dstBegin + i) = *(srcBegin + i);
            }
        }
        else
        {
            std::memcpy(dstBegin, srcBegin, aNBits);
        }
    }

    template <std::size_t BitCount, typename T>
    constexpr static std::array<char, BitCount> addValueExpected(
        std::string_view aDstBitStr, DstOffset aDstOffset, T aValue,
        NumBits aNumBits) noexcept
    {
        static_assert(endian::is_uint_v<T>);
        constexpr auto kValueBitSize = utils::num_bits<T>();
        assert(aNumBits <= kValueBitSize);
        assert(aDstOffset + aNumBits <= aDstBitStr.size());
        assert(aDstOffset < CHAR_BIT);
        assert(BitCount == aDstBitStr.size());

        auto etalonBitArr = utils::make_array<BitCount, char>(aDstBitStr);

        const auto kValueBitsArray = to_symbol_array(value_to_bytes(aValue));
        std::string_view valueBitStr{kValueBitsArray.data(), kValueBitSize};

        std::string_view bitsStrToAdd =
            valueBitStr.substr(valueBitStr.size() - aNumBits.get());

        auto posToPaste = etalonBitArr.data() + aDstOffset.get();

        copy(posToPaste, bitsStrToAdd.data(), n_bytes{aNumBits});

        return etalonBitArr;
    }

    template <typename T>
    constexpr static T getValueExpected(std::string_view aSrcBitStr,
                                        SrcOffset aSrcOffset,
                                        NumBits aNBits) noexcept
    {
        static_assert(endian::is_uint_v<T>, "Invalid T");
        constexpr auto kBitsInValue = utils::num_bits<T>();
        assert(aNBits.get() <= kBitsInValue);
        assert(aSrcOffset.get() <= CHAR_BIT);
        assert(aSrcOffset.get() + aNBits.get() <= aSrcBitStr.size());

        auto valueBitsArr = utils::make_array<char, kBitsInValue>('0');

        auto src = aSrcBitStr.cbegin() + aSrcOffset;
        auto dst = valueBitsArr.begin() + kBitsInValue - aNBits;

        for (std::size_t i = 0; i < aNBits; ++i)
        {
            *(dst + i) = *(src + i);
        }
        std::string_view allValueBitsStr{valueBitsArr.data(),
                                         valueBitsArr.size()};

        std::array<std::byte, sizeof(T)> resultByteArray =
            to_byte_array<kBitsInValue>(allValueBitsStr);
        using ::rabbit::details::bytes_to_value;
        T result = bytes_to_value<T>(Src{resultByteArray.data()});
        return result;
    }

   private:
    template <typename T, std::size_t... I>
    constexpr static T bytes_to_value(Src aBytes,
                                      std::index_sequence<I...>) noexcept
    {
        static_assert(endian::is_uint_v<T>, "Invalid T");
        T result{
            (... | static_cast<T>(static_cast<T>(aBytes[I]) << I * CHAR_BIT))};
        return result;
    }

    template <typename Destination, typename Source>
    static inline constexpr void copy(Destination aDst, Source aSrc,
                                      n_bytes aNBytes) noexcept
    {
        if (__builtin_is_constant_evaluated())
        {
            for (std::size_t i = 0; i < aNBytes; ++i)
            {
                aDst[i] = aSrc[i];
            }
        }
        else
        {
            assert(aDst);
            assert(aSrc);
            std::memcpy(aDst, aSrc, aNBytes);
        }
    }

    template <typename T, std::size_t... I>
    static inline constexpr decltype(auto) value_to_bytes_impl(
        T aValue, std::index_sequence<I...>) noexcept
    {
        return utils::make_array(
            static_cast<std::byte>(aValue >> I * CHAR_BIT)...);
    }

    template <typename T>
    static inline constexpr decltype(auto) value_to_bytes(T aValue) noexcept
    {
        static_assert(endian::is_uint_v<T>, "Invalid T");
        return rabbit::details::to_byte_array(aValue);
    }
};

template <>
class bits<::rabbit::v2::Core>
{
   public:
    using DstOffset = ::rabbit::DstOffset;
    using SrcOffset = ::rabbit::SrcOffset;
    using NumBits = ::rabbit::NumBits;
    using Offset = ::rabbit::Offset;
    using BitIndex = ::rabbit::BitIndex;
    using Dst = ::rabbit::Dst;
    using Src = ::rabbit::Src;
    using n_bytes = ::rabbit::n_bytes;

    struct bb_index_t
    {
        std::size_t byte{};
        std::uint8_t bit{};

        bb_index_t(const bb_index_t &) = default;
        bb_index_t &operator=(const bb_index_t &) = default;
        bb_index_t(bb_index_t &&) = default;
        bb_index_t &operator=(bb_index_t &&) = default;

        template <typename BytePos, typename BitPos>
        inline constexpr bb_index_t(BytePos &&aByte, BitPos &&aBit) noexcept
            : byte{static_cast<std::size_t>(aByte)}
            , bit{static_cast<std::uint8_t>(aBit)}
        {
            if (!__builtin_is_constant_evaluated())
            {
                assert(aBit < CHAR_BIT);
            }
        }

        inline constexpr bb_index_t(BitIndex aBitIndex) noexcept
            : bb_index_t(
                  [aBitIndex]()
                  {
                      constexpr auto kCharBit =
                          static_cast<BitIndex::value_type>(CHAR_BIT);
                      if (__builtin_is_constant_evaluated())
                      {
                          return bb_index_t{aBitIndex / kCharBit,
                                            aBitIndex % kCharBit};
                      }
                      else
                      {
                          const auto divided =
                              std::div(static_cast<int>(aBitIndex), CHAR_BIT);
                          return bb_index_t{divided.quot, divided.rem};
                      }
                  }())
        {
        }
    };

    static inline constexpr std::byte bitAtIndex(std::size_t aBitPos) noexcept
    {
        return std::byte{1} << aBitPos;
    }

    static constexpr void setBit(Dst aData, std::size_t aPos) noexcept
    {
        auto [byte, bit] = bb_index_t(BitIndex{aPos});
        aData[byte] |= bitAtIndex(bit);
    }

    static constexpr void resetBit(Dst aData, std::size_t aPos) noexcept
    {
        auto [byte, bit] = bb_index_t(BitIndex{aPos});
        aData[byte] &= ~bitAtIndex(bit);
    }

    static constexpr bool isBitSet(Src aData, std::size_t aPos) noexcept
    {
        auto [byte, bit] = bb_index_t(BitIndex{aPos});
        return (aData[byte] & bitAtIndex(bit)) != std::byte{0};
    }

    static constexpr void to_byte_buf(Dst aDst, const std::size_t aDstOffset,
                                      std::string_view aBitStr) noexcept
    {
        const auto kNumBits = aBitStr.size();
        for (std::size_t i = 0; i < kNumBits; ++i)
        {
            const auto index = kNumBits - i - 1;
            if (aBitStr[index] == '0')
            {
                resetBit(aDst, aDstOffset + i);
            }
            else
            {
                setBit(aDst, aDstOffset + i);
            }
        }
    }

    static constexpr void to_byte_buf(Dst aDst,
                                      std::string_view aBitStr) noexcept
    {
        to_byte_buf(aDst, 0, aBitStr);
    }

    template <std::size_t NBits>
    static constexpr auto to_byte_array(std::string_view aBitStr) noexcept
    {
        std::array<std::byte, rabbit::details::bytes_count(NumBits{NBits})>
            result{};
        to_byte_buf(Dst{result.data()}, aBitStr);
        return result;
    }

    static constexpr void to_symbol_buf(char *const aDst, Src aSrc,
                                        const std::size_t aNBits) noexcept
    {
        for (std::size_t i = 0; i < aNBits; ++i)
        {
            const auto index = aNBits - i - 1;
            aDst[i] = isBitSet(aSrc, index) ? '1' : '0';
        }
    }

    template <std::size_t NBits, typename ArrayT>
    static constexpr decltype(auto) to_symbol_array(ArrayT &&aArray) noexcept
    {
        static_assert(
            std::is_same_v<utils::std_array_data_t<ArrayT>, std::byte>,
            "Elements of aArray must belong to std::uint8_t type.");
        constexpr std::size_t kMaxNBytes = utils::std_array_size_v<ArrayT>;
        static_assert(NBits <= kMaxNBytes * CHAR_BIT, "Invalid NBits.");
        std::array<char, NBits> result{};
        to_symbol_buf(result.data(), Src{aArray.data()}, NBits);
        return result;
    }

    template <typename ArrayT>
    static constexpr decltype(auto) to_symbol_array(ArrayT &&aArray) noexcept
    {
        constexpr std::size_t kNBytes = utils::std_array_size_v<ArrayT>;
        return to_symbol_array<kNBytes * CHAR_BIT>(
            std::forward<ArrayT>(aArray));
    }

    static constexpr void copy_bits_expected_str(char *const aDst,
                                                 std::size_t aDstSize,
                                                 const DstOffset aDstOffset,
                                                 std::string_view aSrc,
                                                 const SrcOffset aSrcOffset,
                                                 const NumBits aNBits) noexcept
    {
        assert(aDst != nullptr);
        assert(aDstOffset < CHAR_BIT);
        assert(aSrcOffset < CHAR_BIT);
        assert(aDstOffset.get() + aNBits.get() <= aDstSize);
        assert(aSrcOffset.get() + aNBits.get() <= aSrc.size());

        if (!aNBits)
        {
            return;
        }

        char *dstBegin = aDst + aDstSize - aDstOffset - aNBits;
        char const *srcBegin = aSrc.data() + aSrc.size() - aSrcOffset - aNBits;

        if (__builtin_is_constant_evaluated())
        {
            for (std::size_t i = 0; i < aNBits; ++i)
            {
                *(dstBegin + i) = *(srcBegin + i);
            }
        }
        else
        {
            std::memcpy(dstBegin, srcBegin, aNBits);
        }
    }

    template <std::size_t BitCount, typename T>
    constexpr static std::array<char, BitCount> addValueExpected(
        std::string_view aDstBitStr, DstOffset aDstOffset, T aValue,
        NumBits aNumBits) noexcept
    {
        static_assert(endian::is_uint_v<T>);
        constexpr auto kValueBitSize = utils::num_bits<T>();
        assert(aNumBits <= kValueBitSize);
        assert(aDstOffset + aNumBits <= aDstBitStr.size());
        assert(aDstOffset < CHAR_BIT);
        assert(BitCount == aDstBitStr.size());

        auto etalonBitArr = utils::make_array<BitCount, char>(aDstBitStr);

        const auto kValueBitsArray = to_symbol_array(value_to_bytes(aValue));
        std::string_view valueBitStr{kValueBitsArray.data(), kValueBitSize};

        std::string_view bitsStrToAdd =
            valueBitStr.substr(valueBitStr.size() - aNumBits.get());

        auto posToPaste =
            etalonBitArr.data() + etalonBitArr.size() - aDstOffset - aNumBits;

        copy(posToPaste, bitsStrToAdd.data(), n_bytes{aNumBits});

        return etalonBitArr;
    }

    template <typename T>
    constexpr static T getValueExpected(std::string_view aSrcBitStr,
                                        SrcOffset aSrcOffset,
                                        NumBits aNBits) noexcept
    {
        static_assert(endian::is_uint_v<T>, "Invalid T");
        constexpr auto kBitsInValue = utils::num_bits<T>();
        assert(aNBits.get() <= kBitsInValue);
        assert(aSrcOffset.get() <= CHAR_BIT);
        const std::size_t kTotalBits = aSrcOffset.get() + aNBits.get();
        assert(kTotalBits <= aSrcBitStr.size());

        auto valueBitsArr = utils::make_array<char, kBitsInValue>('0');

        auto src = aSrcBitStr.cbegin() + aSrcBitStr.size() - kTotalBits;
        auto dst = valueBitsArr.begin() + kBitsInValue - aNBits;

        for (std::size_t i = 0; i < aNBits; ++i)
        {
            *(dst + i) = *(src + i);
        }
        std::string_view allValueBitsStr{valueBitsArr.data(),
                                         valueBitsArr.size()};

        std::array<std::byte, sizeof(T)> resultByteArray =
            to_byte_array<kBitsInValue>(allValueBitsStr);

        using Indices = std::make_index_sequence<sizeof(T)>;
        T result = bytes_to_value<T>(Src{resultByteArray.data()}, Indices{});
        return result;
    }

   private:
    template <typename T, std::size_t... I>
    constexpr static T bytes_to_value(Src aBytes,
                                      std::index_sequence<I...>) noexcept
    {
        static_assert(endian::is_uint_v<T>, "Invalid T");
        T result =
            (... | static_cast<T>(static_cast<T>(aBytes[I]) << I * CHAR_BIT));
        return result;
    }

    template <typename Destination, typename Source>
    static inline constexpr void copy(Destination aDst, Source aSrc,
                                      n_bytes aNBytes) noexcept
    {
        if (__builtin_is_constant_evaluated())
        {
            for (std::size_t i = 0; i < aNBytes; ++i)
            {
                aDst[i] = aSrc[i];
            }
        }
        else
        {
            assert(aDst);
            assert(aSrc);
            std::memcpy(aDst, aSrc, aNBytes);
        }
    }

    template <typename T, std::size_t... I>
    static inline constexpr decltype(auto) value_to_bytes_impl(
        T aValue, std::index_sequence<I...>) noexcept
    {
        return utils::make_array(
            static_cast<std::byte>(aValue >> I * CHAR_BIT)...);
    }

    template <typename T>
    static inline constexpr decltype(auto) value_to_bytes(T aValue) noexcept
    {
        static_assert(endian::is_uint_v<T>, "Invalid T");
        return value_to_bytes_impl(aValue,
                                   std::make_index_sequence<sizeof(T)>{});
    }
};
}  // namespace test_utils

#endif /* rabbit_test_helpers_h */
