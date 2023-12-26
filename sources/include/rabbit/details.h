#ifndef rabbit_details_h
#define rabbit_details_h

#include <endian/endian.h>
#include <utils/utils.h>

#include <cassert>

#include "bit_ops.h"
#include "typedefs.h"

namespace rabbit
{
namespace details
{
template <typename ImplT>
class IndexBase
{
   public:
    template <typename T, std::size_t I>
    static constexpr std::size_t get() noexcept
    {
        return ImplT::template getImpl<T>(I);
    }

    template <typename T>
    static constexpr std::size_t get(std::size_t I) noexcept
    {
        return ImplT::template getImpl<T>(I);
    }
};

template <endian::eEndian>
class IndexImpl;

template <>
class IndexImpl<endian::eEndian::kLittle>
    : public IndexBase<IndexImpl<endian::eEndian::kLittle>>
{
    friend class IndexBase<IndexImpl<endian::eEndian::kLittle>>;

   private:
    template <typename U>
    static constexpr std::size_t getImpl(std::size_t I) noexcept
    {
        using T = std::decay_t<U>;
        static_assert(endian::is_uint_v<T>,
                      "T must be unsigned integer but not bool");
        assert(I < sizeof(T) && "Invalid I");
        return sizeof(T) - 1 - I;
    }
};

template <>
class IndexImpl<endian::eEndian::kBig>
    : public IndexBase<IndexImpl<endian::eEndian::kBig>>
{
    friend class IndexBase<IndexImpl<endian::eEndian::kBig>>;

   private:
    template <typename T>
    static constexpr std::size_t getImpl(std::size_t I) noexcept
    {
        assert(I < sizeof(T) && "Invalid I");
        return I;
    }
};

template <typename T, std::size_t I>
inline constexpr std::size_t CIndex =
    details::IndexImpl<endian::kValue>::get<T, I>();

template <typename T>
constexpr std::size_t Index(std::size_t I) noexcept
{
    return details::IndexImpl<endian::kValue>::get<T>(I);
}

template <typename U>
constexpr auto to_byte_array(U &&aValue) noexcept
{
    using T = std::decay_t<U>;
    static_assert(endian::is_uint_v<T>,
                  "T must be unsigned integer type and not bool.");
    std::array<std::byte, sizeof(T)> valueAsBytes{};
    if (__builtin_is_constant_evaluated())
    {
        for (std::size_t i = 0; i < sizeof(T); ++i)
        {
            valueAsBytes[sizeof(T) - 1 - i] =
                static_cast<std::byte>(aValue >> i * CHAR_BIT);
        }
    }
    else
    {
        const auto netOrdered = endian::toNet(std::forward<U>(aValue));
        std::memcpy(valueAsBytes.data(), &netOrdered, sizeof(T));
    }
    return valueAsBytes;
}

template <typename T>
constexpr T bytes_to_value(Src aBuf,
                           const std::size_t aNBytes = sizeof(T)) noexcept
{
    static_assert(endian::is_uint_v<T>,
                  "T must be unsigned integer type and not bool.");
    assert(aNBytes <= sizeof(T));
    T result{};
    if (__builtin_is_constant_evaluated())
    {
        for (std::size_t i = 0; i < aNBytes; ++i)
        {
            result |= static_cast<T>(static_cast<T>(aBuf[i])
                                     << (sizeof(T) - 1 - i) * CHAR_BIT);
        }
    }
    else
    {
        assert(aBuf);
        std::memcpy(&result, aBuf, aNBytes);
        result = endian::toHost(result);
    }
    return result;
}

enum class eAlign : bool
{
    kLeft = false,
    kRight = true
};

static inline constexpr void copy(Dst aDst, Src aSrc, n_bytes aNBytes) noexcept
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

template <typename T>
constexpr std::byte byteAt(T &&aValue, std::size_t aIndex) noexcept
{
    using PureT = utils::remove_cvref_t<T>;
    assert((aIndex < sizeof(PureT)) && "Invalid aIndex.");
    return static_cast<std::byte>(
        static_cast<PureT>(aValue << aIndex * CHAR_BIT) >>
        (sizeof(T) - 1) * CHAR_BIT);
}

template <std::size_t I, typename T>
constexpr std::byte byteAt(T &&aValue) noexcept
{
    return byteAt(std::forward<T>(aValue), I);
}

template <typename T>
constexpr void addValue(Dst aDst, T &&aValue, std::size_t aNBytes) noexcept
{
    for (std::size_t i = 0; i < aNBytes; ++i)
    {
        aDst[i] = byteAt(aValue, i);
    }
}

template <typename T, std::size_t... I>
constexpr void addValue(Dst aDst, T &&aValue,
                        std::index_sequence<I...>) noexcept
{
    ((*(aDst + I) = byteAt<I>(aValue)), ...);
}

template <typename T, std::size_t... I>
constexpr void addNLeastSignificantBytesImpl([[maybe_unused]] Dst aDst,
                                             T &&aValue,
                                             std::index_sequence<I...>) noexcept
{
    constexpr std::size_t kNBytes = sizeof(T) - sizeof...(I);
    ((*(aDst + I) = byteAt<kNBytes + I>(aValue)), ...);
}

template <typename T, std::size_t N>
constexpr void addNLeastSignificantBytes(Dst aDst, T &&aValue) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(N <= sizeof(UIntT), "Too big N.");
    using Indices = std::make_index_sequence<N>;
    addNLeastSignificantBytesImpl(aDst, std::forward<T>(aValue), Indices{});
}

template <typename T>
constexpr decltype(auto) highNBitsWithOffset(T &&aValue,
                                             std::uint_fast8_t aNBits,
                                             std::uint_fast8_t aOffset) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(endian::is_uint_v<UIntT>,
                  "UIntT must be unsigned integer type.");
    assert((aOffset + aNBits <= CHAR_BIT * sizeof(UIntT)) &&
           "UIntT cannot contain <aOffset + aNBits> bits");
    const auto kOffset = CHAR_BIT * sizeof(UIntT) - aNBits;
    const UIntT lowBitsErased = aValue >> kOffset;
    const UIntT result =
        static_cast<UIntT>(lowBitsErased << (kOffset - aOffset));
    return result;
}

template <typename T>
constexpr decltype(auto) highNBits(T &&aValue, NumBits aNBits) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(std::disjunction_v<utils::is_uint<UIntT>,
                                     std::is_same<UIntT, std::byte>>);
    assert((aNBits <= CHAR_BIT * sizeof(UIntT)) && "Invalid aNBits");
    const auto kOffset = CHAR_BIT * sizeof(UIntT) - aNBits;
    const UIntT lowBitsErased = aValue >> kOffset;
    const UIntT result = static_cast<UIntT>(lowBitsErased << kOffset);
    return result;
}

template <typename T>
constexpr decltype(auto) lowNBits(T &&aValue, NumBits aNBits) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(endian::is_uint_v<UIntT>,
                  "UIntT must be unsigned integer type.");
    assert((aNBits <= CHAR_BIT * sizeof(UIntT)) && "Invalid aNBits");
    const auto kOffset = CHAR_BIT * sizeof(UIntT) - aNBits;
    const UIntT highBitsErased = static_cast<UIntT>(aValue << kOffset);
    const UIntT result = highBitsErased >> kOffset;
    return result;
}

template <typename T, eAlign kAlign>
constexpr T get(Src aSrc, const std::size_t aNBytes) noexcept
{
    static_assert(endian::is_uint_v<T>,
                  "T must be unsigned integer type. And bool is forbidden.");
    assert(aNBytes <= sizeof(T) && "T is not enough to contain aNBytes bytes");
    T value{};
    const auto kBytes = static_cast<std::ptrdiff_t>(aNBytes);
    if constexpr (kAlign == eAlign::kRight)
    {
        for (std::ptrdiff_t i = 0; i < kBytes; ++i)
        {
            value |= static_cast<T>(static_cast<T>(*(aSrc + i))
                                    << ((kBytes - i - 1) * CHAR_BIT));
        }
        return value;
    }
    else
    {
        constexpr auto T_size = static_cast<std::ptrdiff_t>(sizeof(T));
        for (std::ptrdiff_t i = 0; i < kBytes; ++i)
        {
            value |= static_cast<T>(static_cast<T>(*(aSrc + i))
                                    << ((T_size - i - 1) * CHAR_BIT));
        }
        return value;
    }
}

template <typename T, eAlign kAlign, std::size_t... I>
constexpr T get_impl(Src aSrc, std::index_sequence<I...>) noexcept
{
    static_assert(
        std::disjunction_v<utils::is_uint<T>, std::is_same<T, std::byte>>);
    constexpr std::size_t NBytes = sizeof...(I);
    static_assert(NBytes <= sizeof(T),
                  "T is not enough to contain NBytes bytes");
    if constexpr (kAlign == eAlign::kRight)
    {
        return static_cast<T>(
            (... | static_cast<T>(static_cast<T>(*(aSrc + I))
                                  << ((NBytes - I - 1) * CHAR_BIT))));
    }
    else
    {
        return static_cast<T>(
            (... | static_cast<T>(static_cast<T>(*(aSrc + I))
                                  << ((sizeof(T) - I - 1) * CHAR_BIT))));
    }
}

template <typename T, std::size_t NBytes, eAlign kAlign>
constexpr T get(Src aSrc) noexcept
{
    using Indices = std::make_index_sequence<NBytes>;
    return get_impl<T, kAlign>(aSrc, Indices{});
}

template <typename T1, typename T2>
constexpr decltype(auto) addHighBits(T1 &&aTo, T2 &&aFrom,
                                     NumBits aNBits) noexcept
{
    using ::rabbit::bit_ops::compose;
    using ::rabbit::bit_ops::mask_left;
    using UIntT = utils::remove_cvref_t<T1>;
    static_assert(std::disjunction_v<utils::is_uint<UIntT>,
                                     std::is_same<UIntT, std::byte>>);
    static_assert(std::is_same_v<UIntT, utils::remove_cvref_t<T2>>,
                  "Invalid T2");
    if (!aNBits)
    {
        return UIntT{aTo};
    }
    constexpr std::size_t kBitsInValue = sizeof(UIntT) * CHAR_BIT;
    assert(aNBits <= kBitsInValue);
    const auto kOffset = static_cast<Offset::value_type>(kBitsInValue - aNBits);
    auto lMask = mask_left<UIntT>(Offset{kOffset});
    return compose(std::forward<T1>(aTo), std::forward<T2>(aFrom),
                   std::move(lMask));
}

template <typename T1, typename T2>
constexpr decltype(auto) addLowBits(T1 &&aTo, T2 &&aFrom,
                                    std::uint_fast8_t aNBits) noexcept
{
    using ::rabbit::bit_ops::compose;
    using ::rabbit::bit_ops::mask_right;
    using UIntT = utils::remove_cvref_t<T1>;
    static_assert(std::disjunction_v<utils::is_uint<UIntT>,
                                     std::is_same<UIntT, std::byte>>);
    static_assert(std::is_same_v<UIntT, utils::remove_cvref_t<T2>>,
                  "Invalid T2");
    if (!aNBits)
    {
        return UIntT{aTo};
    }
    constexpr std::size_t kBitsInValue = sizeof(UIntT) * CHAR_BIT;
    assert(aNBits <= kBitsInValue);
    const auto kOffset = static_cast<Offset::value_type>(kBitsInValue - aNBits);
    auto rMask = mask_right<UIntT>(Offset{kOffset});
    return compose(std::forward<T1>(aTo), std::forward<T2>(aFrom),
                   std::move(rMask));
}

//! Add lowest aNBits from aValue to aDst
/*!
  \param aDst a pointer to memory where aValue will be written
  \param aNBits number of bits in aValue which must be saved.
  \param aValue an unsigned integer to serialize
  \note Its presumed that high sizeof(aValue)*CHAR_BIT - aNBits bits from
  aValue are zeroed
*/
template <typename T>
constexpr void addUInt64High(Dst aDst, NumBits aNBits, T &&aValue) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(std::is_same_v<UIntT, std::uint64_t>,
                  "UIntT should be equal to std::uint64_t");
    const auto kOffset = num_bits<UIntT> - aNBits;
    assert(kOffset < CHAR_BIT);
    assert(kOffset > 0);
    assert(highNBits(byteAt<0>(aValue), kOffset) == std::byte{0});
    *aDst = byteAt<0>(aValue) | highNBits(*aDst, kOffset);
    using Indices = utils::shifted_sequence_t<
        std::make_index_sequence<static_cast<std::size_t>(sizeof(UIntT) - 1)>,
        static_cast<std::size_t>(1)>;
    addValue(aDst, std::forward<T>(aValue), Indices{});
}

constexpr void addUInt64Low(Dst aDst, NumBits aNBits,
                            std::uint8_t aValue) noexcept
{
    assert(aNBits < CHAR_BIT);
    assert(aNBits > 0);
    const std::byte leftShiftedDst = *aDst << aNBits.get();
    const std::byte dstVal = leftShiftedDst >> aNBits.get();
    const auto srcVal = static_cast<std::byte>(aValue << (CHAR_BIT - aNBits));
    *aDst = dstVal | srcVal;
}

template <typename T>
constexpr void add(Dst aDst, T &&aValue) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    using Indices = std::make_index_sequence<sizeof(UIntT)>;
    addValue(aDst, std::forward<T>(aValue), Indices{});
}

constexpr inline std::size_t bytes_count(NumBits aNBits) noexcept
{
    constexpr auto kCharBit = static_cast<NumBits::value_type>(CHAR_BIT);
    const std::size_t kNBytes =
        aNBits / kCharBit + ((aNBits % kCharBit != 0) ? 1 : 0);
    return kNBytes;
}

constexpr inline std::size_t bytes_count(NumBits aNBits,
                                         Offset aOffset) noexcept
{
    assert(aOffset < CHAR_BIT && "Invalid aOffset.");
    return bytes_count(NumBits{aOffset} + aNBits);
}

constexpr inline std::size_t bytes_count(NumBits aNBits,
                                         DstOffset aOffset) noexcept
{
    assert(aOffset < CHAR_BIT && "Invalid aOffset.");
    return bytes_count(NumBits{aOffset} + aNBits);
}

constexpr inline std::size_t bytes_count(NumBits aNBits,
                                         SrcOffset aOffset) noexcept
{
    assert(aOffset < CHAR_BIT && "Invalid aOffset.");
    return bytes_count(NumBits{aOffset} + aNBits);
}

template <typename T>
constexpr void addBits(Dst aDst, Offset aOffset, NumBits aNBits,
                       T &&aValue) noexcept
{
    using ::rabbit::bit_ops::compose;
    using ::rabbit::bit_ops::mask;
    const std::size_t NBytes = bytes_count(aNBits, aOffset);
    assert(NBytes > 0 && "NBytes must be positive");
    using UIntT = utils::remove_cvref_t<T>;
    static_assert(endian::is_uint_v<UIntT>, "UIntT must be unsigned integer");
    assert(NBytes <= sizeof(UIntT) && "Invalid NBytes");
    const auto kMask = mask<UIntT>(aOffset, aNBits);
    const auto kDst = get<UIntT, eAlign::kLeft>(Src{aDst}, NBytes);
    UIntT result =
        compose(std::move(kDst), std::forward<T>(aValue), std::move(kMask));
    addValue(aDst, std::move(result), NBytes);
}

template <std::size_t NBytes, typename T>
constexpr void addBits(Dst aDst, Offset aOffset, NumBits aNBits,
                       T &&aValue) noexcept
{
    using ::rabbit::bit_ops::compose;
    using ::rabbit::bit_ops::mask;
    static_assert(NBytes > 0, "NBytes must be positive");
    using UIntT = utils::remove_cvref_t<T>;
    static_assert(std::disjunction_v<utils::is_uint<UIntT>,
                                     std::is_same<UIntT, std::byte>>);
    static_assert(NBytes <= sizeof(UIntT), "Invalid NBytes");
    const auto kMask = mask<UIntT>(aOffset, aNBits);
    const auto kDst = get<UIntT, NBytes, eAlign::kLeft>(Src{aDst});
    UIntT result =
        compose(std::move(kDst), std::forward<T>(aValue), std::move(kMask));
    using Indices = std::make_index_sequence<NBytes>;
    addValue(aDst, std::move(result), Indices{});
}
}  // namespace details
}  // namespace rabbit

#endif /* rabbit_details_h */
