#ifndef rabbit_details_h
#define rabbit_details_h

#include <cassert>

#include "endian.h"
#include "utils.h"

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

template <eEndian>
class IndexImpl;

template <>
class IndexImpl<eEndian::kLittle>
    : public IndexBase<IndexImpl<eEndian::kLittle>>
{
    friend class IndexBase<IndexImpl<eEndian::kLittle>>;

   private:
    template <typename U>
    static constexpr std::size_t getImpl(std::size_t I) noexcept
    {
        using T = std::decay_t<U>;
        static_assert(is_uint_v<T>, "T must be unsigned integer but not bool");
        assert(I < sizeof(T) && "Invalid I");
        return sizeof(T) - 1 - I;
    }
};

template <>
class IndexImpl<eEndian::kBig> : public IndexBase<IndexImpl<eEndian::kBig>>
{
    friend class IndexBase<IndexImpl<eEndian::kBig>>;

   private:
    template <typename T>
    static constexpr std::size_t getImpl(std::size_t I) noexcept
    {
        assert(I < sizeof(T) && "Invalid I");
        return I;
    }
};

template <typename T, std::size_t I>
inline constexpr std::size_t CIndex = details::IndexImpl<kEndian>::get<T, I>();

template <typename T>
constexpr std::size_t Index(std::size_t I) noexcept
{
    return details::IndexImpl<kEndian>::get<T>(I);
}

template <typename U>
constexpr auto to_uint8_array(U &&aValue) noexcept
{
    using T = std::decay_t<U>;
    static_assert(is_uint_v<T>,
                  "T must be unsigned integer type and not bool.");
    std::array<uint8_t, sizeof(T)> valueAsBytes{};
    if (__builtin_is_constant_evaluated())
    {
        for (std::size_t i = 0; i < sizeof(T); ++i)
        {
            valueAsBytes[Index<T>(i)] =
                static_cast<uint8_t>(aValue >> i * CHAR_BIT);
        }
    }
    else
    {
        const auto netOrdered = rabbit::toNet(std::forward<U>(aValue));
        std::memcpy(valueAsBytes.data(), &netOrdered, sizeof(T));
    }
    return valueAsBytes;
}

template <typename T>
constexpr T uint8_buf_to_value(uint8_t const *const aBuf,
                               const std::size_t aNBytes = sizeof(T)) noexcept
{
    static_assert(is_uint_v<T>,
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
        result = rabbit::toHost(result);
    }
    return result;
}

enum class eAlign : bool
{
    kLeft = false,
    kRight = true
};

template <typename T>
static constexpr void copy(T *const aDst, T const *const aSrc,
                           std::size_t NBytes) noexcept
{
    if (__builtin_is_constant_evaluated())
    {
        for (std::size_t i = 0; i < NBytes; ++i)
        {
            *(aDst + i) = *(aSrc + i);
        }
    }
    else
    {
        assert(aDst);
        assert(aSrc);
        std::memcpy(aDst, aSrc, NBytes);
    }
}

template <typename T>
constexpr T mask(std::size_t aOffset, std::size_t aNBits) noexcept
{
    static_assert(is_uint_v<T>, "T must be unsigned integer");
    assert((aNBits + aOffset <= CHAR_BIT * sizeof(T)) &&
           "error: aNBits + aOffset <= CHAR_BIT * sizeof(T) isn't satisfied");
    constexpr T kBits = static_cast<T>(~T{});
    const T kLAlignedMask =
        static_cast<T>(kBits << (sizeof(T) * CHAR_BIT - aNBits));
    const T kMask = kLAlignedMask >> aOffset;
    return kMask;
}

template <typename T>
constexpr T invertedMask(uint_fast8_t aOffset, uint_fast8_t aNBits) noexcept
{
    return ~mask<T>(aOffset, aNBits);
}

template <typename T>
constexpr T maskR(std::size_t aOffset) noexcept
{
    static_assert(is_uint_v<T>, "T must be unsigned integer");
    assert((aOffset <= CHAR_BIT * sizeof(T)) && "Invalid aNBits.");
    constexpr T kSetBits = static_cast<T>(~T{});
    const T kMask = static_cast<T>(kSetBits >> aOffset);
    return kMask;
}

template <typename T>
constexpr T maskL(std::size_t aOffset) noexcept
{
    static_assert(is_uint_v<T>, "T must be unsigned integer");
    assert((aOffset <= CHAR_BIT * sizeof(T)) && "Invalid aNBits.");
    constexpr T kSetBits = static_cast<T>(~T{});
    const T kMask = static_cast<T>(kSetBits << aOffset);
    return kMask;
}

template <typename T>
constexpr uint8_t byteAt(T &&aValue, std::size_t aIndex) noexcept
{
    using PureT = remove_cvref_t<T>;
    assert((aIndex < sizeof(PureT)) && "Invalid aIndex.");
    return static_cast<uint8_t>(
        static_cast<PureT>(aValue << aIndex * CHAR_BIT) >>
        (sizeof(T) - 1) * CHAR_BIT);
}

template <std::size_t I, typename T>
constexpr uint8_t byteAt(T &&aValue) noexcept
{
    return byteAt(std::forward<T>(aValue), I);
}

template <typename T>
constexpr void addValue(uint8_t *const aDst, T &&aValue,
                        std::size_t aNBytes) noexcept
{
    for (std::size_t i = 0; i < aNBytes; ++i)
    {
        *(aDst + i) = byteAt(aValue, i);
    }
}

template <typename T, std::size_t... I>
constexpr void addValue(uint8_t *const aDst, T &&aValue,
                        std::index_sequence<I...>) noexcept
{
    ((*(aDst + I) = byteAt<I>(aValue)), ...);
}

template <typename T, std::size_t... I>
constexpr void addNLeastSignificantBytesImpl(uint8_t *const aDst, T &&aValue,
                                             std::index_sequence<I...>) noexcept
{
    constexpr std::size_t kNBytes = sizeof(T) - sizeof...(I);
    ((*(aDst + I) = byteAt<kNBytes + I>(aValue)), ...);
}

template <typename T, std::size_t N>
constexpr void addNLeastSignificantBytes(uint8_t *const aDst,
                                         T &&aValue) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(N <= sizeof(UIntT), "Too big N.");
    using Indices = std::make_index_sequence<N>;
    addNLeastSignificantBytesImpl(aDst, std::forward<T>(aValue), Indices{});
}

template <typename T>
constexpr decltype(auto) highNBitsWithOffset(T &&aValue, uint_fast8_t aNBits,
                                             uint_fast8_t aOffset) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(is_uint_v<UIntT>, "UIntT must be unsigned integer type.");
    assert((aOffset + aNBits <= CHAR_BIT * sizeof(UIntT)) &&
           "UIntT cannot contain <aOffset + aNBits> bits");
    const auto kOffset = CHAR_BIT * sizeof(UIntT) - aNBits;
    const UIntT lowBitsErased = aValue >> kOffset;
    const UIntT result =
        static_cast<UIntT>(lowBitsErased << (kOffset - aOffset));
    return result;
}

template <typename T>
constexpr decltype(auto) highNBits(T &&aValue, uint_fast8_t aNBits) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(is_uint_v<UIntT>, "UIntT must be unsigned integer type.");
    assert((aNBits <= CHAR_BIT * sizeof(UIntT)) && "Invalid aNBits");
    const auto kOffset = CHAR_BIT * sizeof(UIntT) - aNBits;
    const UIntT lowBitsErased = aValue >> kOffset;
    const UIntT result = static_cast<UIntT>(lowBitsErased << kOffset);
    return result;
}

template <typename T>
constexpr decltype(auto) lowNBits(T &&aValue, uint_fast8_t aNBits) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(is_uint_v<UIntT>, "UIntT must be unsigned integer type.");
    assert((aNBits <= CHAR_BIT * sizeof(UIntT)) && "Invalid aNBits");
    const auto kOffset = CHAR_BIT * sizeof(UIntT) - aNBits;
    const UIntT highBitsErased = static_cast<UIntT>(aValue << kOffset);
    const UIntT result = highBitsErased >> kOffset;
    return result;
}

template <typename T, eAlign kAlign>
constexpr T get(uint8_t const *const aSrc, const std::size_t aNBytes) noexcept
{
    static_assert(is_uint_v<T>,
                  "T must be unsigned integer type. And bool is forbidden.");
    assert(aNBytes <= sizeof(T) && "T is not enough to contain aNBytes bytes");
    T value{};
    if constexpr (kAlign == eAlign::kRight)
    {
        for (std::size_t i = 0; i < aNBytes; ++i)
        {
            value |= static_cast<T>(static_cast<T>(*(aSrc + i))
                                    << ((aNBytes - i - 1) * CHAR_BIT));
        }
        return value;
    }
    else
    {
        for (std::size_t i = 0; i < aNBytes; ++i)
        {
            value |= static_cast<T>(static_cast<T>(*(aSrc + i))
                                    << ((sizeof(T) - i - 1) * CHAR_BIT));
        }
        return value;
    }
}

template <typename T, eAlign kAlign, std::size_t... I>
constexpr T get_impl(uint8_t const *const aSrc,
                     std::index_sequence<I...>) noexcept
{
    static_assert(is_uint_v<T>,
                  "T must be unsigned integer type. And bool is forbidden.");
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
constexpr T get(uint8_t const *const aSrc) noexcept
{
    using Indices = std::make_index_sequence<NBytes>;
    return get_impl<T, kAlign>(aSrc, Indices{});
}

template <typename T1, typename T2, typename T3>
constexpr decltype(auto) composition(T1 &&aDst, T2 &&aSrc, T3 &&aMask) noexcept
{
    using UIntT = remove_cvref_t<T1>;
    static_assert(
        is_uint_v<UIntT>,
        "UIntT must be unsigned integer type. And bool is forbidden.");
    static_assert(std::is_same_v<UIntT, remove_cvref_t<T2>>, "Invalid T2");
    static_assert(std::is_same_v<UIntT, remove_cvref_t<T3>>, "Invalid T3");
    UIntT result = static_cast<UIntT>((aMask & aSrc) | (~aMask & aDst));
    return result;
}

template <typename T1, typename T2>
constexpr decltype(auto) addHighBits(T1 &&aTo, T2 &&aFrom,
                                     uint_fast8_t aNBits) noexcept
{
    using UIntT = remove_cvref_t<T1>;
    static_assert(
        is_uint_v<UIntT>,
        "UIntT must be unsigned integer type. And bool is forbidden.");
    static_assert(std::is_same_v<UIntT, remove_cvref_t<T2>>, "Invalid T2");
    constexpr std::size_t kBitsInValue = sizeof(UIntT) * CHAR_BIT;
    assert(aNBits <= kBitsInValue);
    auto lMask = maskL<UIntT>(kBitsInValue - aNBits);
    return composition(std::forward<T1>(aTo), std::forward<T2>(aFrom),
                       std::move(lMask));
}

template <typename T1, typename T2>
constexpr decltype(auto) addLowBits(T1 &&aTo, T2 &&aFrom,
                                    uint_fast8_t aNBits) noexcept
{
    using UIntT = remove_cvref_t<T1>;
    static_assert(
        is_uint_v<UIntT>,
        "UIntT must be unsigned integer type. And bool is forbidden.");
    static_assert(std::is_same_v<UIntT, remove_cvref_t<T2>>, "Invalid T2");
    constexpr std::size_t kBitsInValue = sizeof(UIntT) * CHAR_BIT;
    assert(aNBits <= kBitsInValue);
    auto rMask = maskR<UIntT>(kBitsInValue - aNBits);
    return composition(std::forward<T1>(aTo), std::forward<T2>(aFrom),
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
constexpr void addUInt64High(uint8_t *const aDst, const uint_fast8_t aNBits,
                             T &&aValue) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(std::is_same_v<UIntT, uint64_t>,
                  "UIntT should be equal to uint64_t");
    const uint_fast8_t kOffset = sizeof(UIntT) * CHAR_BIT - aNBits;
    assert(kOffset < CHAR_BIT);
    assert(kOffset > 0);
    assert(highNBits(byteAt<0>(aValue), kOffset) == 0);
    *aDst = byteAt<0>(aValue) | highNBits(*aDst, kOffset);
    using Indices = shifted_sequence_t<
        std::make_index_sequence<static_cast<std::size_t>(sizeof(UIntT) - 1)>,
        static_cast<std::size_t>(1)>;
    addValue(aDst, std::forward<T>(aValue), Indices{});
}

constexpr void addUInt64Low(uint8_t *const aDst, uint_fast8_t aNBits,
                            uint8_t aValue) noexcept
{
    assert(aNBits < CHAR_BIT);
    assert(aNBits > 0);
    const uint8_t leftShiftedDst = static_cast<uint8_t>(*aDst << aNBits);
    const uint8_t dstVal = leftShiftedDst >> aNBits;
    const uint8_t srcVal = static_cast<uint8_t>(aValue << (CHAR_BIT - aNBits));
    *aDst = dstVal | srcVal;
}

template <typename T>
constexpr void add(uint8_t *const aDst, T &&aValue) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    using Indices = std::make_index_sequence<sizeof(UIntT)>;
    addValue(aDst, std::forward<T>(aValue), Indices{});
}

constexpr std::size_t bytesCount(std::size_t aNBits) noexcept
{
    const std::size_t kNBytes =
        aNBits / CHAR_BIT + ((aNBits % CHAR_BIT != 0) ? 1 : 0);
    return kNBytes;
}

constexpr std::size_t bytesCount(uint_fast8_t aOffset,
                                 std::size_t aNBits) noexcept
{
    assert(aOffset < CHAR_BIT && "Invalid aOffset.");
    return bytesCount(aOffset + aNBits);
}

template <typename T>
constexpr void addBits(uint8_t *const aDst, uint_fast8_t aOffset,
                       std::size_t aNBits, T &&aValue) noexcept
{
    const std::size_t NBytes = bytesCount(aOffset, aNBits);
    assert(NBytes > 0 && "NBytes must be positive");
    using UIntT = remove_cvref_t<T>;
    static_assert(is_uint_v<UIntT>, "UIntT must be unsigned integer");
    assert(NBytes <= sizeof(UIntT) && "Invalid NBytes");
    const auto kMask = mask<UIntT>(aOffset, aNBits);
    const auto kDst = get<UIntT, eAlign::kLeft>(aDst, NBytes);
    UIntT result =
        composition(std::move(kDst), std::forward<T>(aValue), std::move(kMask));
    addValue(aDst, std::move(result), NBytes);
}

template <std::size_t NBytes, typename T>
constexpr void addBits(uint8_t *const aDst, uint_fast8_t aOffset,
                       std::size_t aNBits, T &&aValue) noexcept
{
    static_assert(NBytes > 0, "NBytes must be positive");
    using UIntT = remove_cvref_t<T>;
    static_assert(is_uint_v<UIntT>, "UIntT must be unsigned integer");
    static_assert(NBytes <= sizeof(UIntT), "Invalid NBytes");
    const auto kMask = mask<UIntT>(aOffset, aNBits);
    const auto kDst = get<UIntT, NBytes, eAlign::kLeft>(aDst);
    UIntT result =
        composition(std::move(kDst), std::forward<T>(aValue), std::move(kMask));
    using Indices = std::make_index_sequence<NBytes>;
    addValue(aDst, std::move(result), Indices{});
}
}  // namespace details
}  // namespace rabbit

#endif /* rabbit_details_h */