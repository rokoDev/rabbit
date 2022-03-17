#ifndef rabbit_details_h
#define rabbit_details_h

#include "endian.h"

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
    template <typename T>
    static constexpr std::size_t getImpl(std::size_t I) noexcept
    {
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

class Operation final
{
   public:
    template <typename T>
    static constexpr T mask(std::size_t aOffset, std::size_t aNBits) noexcept
    {
        static_assert(is_uint_v<T>, "T must be unsigned integer");
        assert(
            (aNBits + aOffset <= CHAR_BIT * sizeof(T)) &&
            "error: aNBits + aOffset <= CHAR_BIT * sizeof(T) isn't satisfied");
        constexpr T kBits = static_cast<T>(~T{});
        const T kLAlignedMask =
            static_cast<T>(kBits << (sizeof(T) * CHAR_BIT - aNBits));
        const T kMask = kLAlignedMask >> aOffset;
        return kMask;
    }

    template <typename T>
    static constexpr T invertedMask(uint_fast8_t aOffset,
                                    uint_fast8_t aNBits) noexcept
    {
        return ~mask<T>(aOffset, aNBits);
    }

    template <typename T>
    static constexpr T maskR(std::size_t aOffset) noexcept
    {
        static_assert(is_uint_v<T>, "T must be unsigned integer");
        assert((aOffset <= CHAR_BIT * sizeof(T)) && "Invalid aNBits.");
        constexpr T kSetBits = static_cast<T>(~T{});
        const T kMask = static_cast<T>(kSetBits >> aOffset);
        return kMask;
    }

    template <typename T>
    static constexpr T maskL(std::size_t aOffset) noexcept
    {
        static_assert(is_uint_v<T>, "T must be unsigned integer");
        assert((aOffset <= CHAR_BIT * sizeof(T)) && "Invalid aNBits.");
        constexpr T kSetBits = static_cast<T>(~T{});
        const T kMask = static_cast<T>(kSetBits << aOffset);
        return kMask;
    }

    template <typename T>
    static constexpr decltype(auto) bytePtrAt(T &&aValue,
                                              std::size_t aIndex) noexcept
    {
        using PureT = remove_cvref_t<T>;
        assert(aIndex < sizeof(PureT) && "Invalid index.");
        if constexpr (std::is_const_v<std::remove_reference_t<T>>)
        {
            return reinterpret_cast<uint8_t const *>(&aValue) +
                   Index<PureT>(aIndex);
        }
        else
        {
            return reinterpret_cast<uint8_t *>(&aValue) + Index<PureT>(aIndex);
        }
    }

    template <std::size_t I, typename T>
    static constexpr decltype(auto) bytePtrAt(T &&aValue) noexcept
    {
        return bytePtrAt(std::forward<T>(aValue), I);
    }

    template <std::size_t I, typename T>
    static constexpr uint8_t byteAt(T &&aValue) noexcept
    {
        return *bytePtrAt<I>(aValue);
    }

    template <typename T>
    static constexpr uint8_t byteAt(T &&aValue, std::size_t aIndex) noexcept
    {
        return *bytePtrAt(std::forward<T>(aValue), aIndex);
    }

    template <typename T>
    static constexpr void addValue(uint8_t *aDst, T &&aValue,
                                   uint_fast8_t aNBytes) noexcept
    {
        for (std::size_t i = 0; i < aNBytes; ++i)
        {
            *(aDst + i) = byteAt(aValue, i);
        }
    }

    template <typename T, std::size_t... I>
    static constexpr void addValue(uint8_t *aDst, T &&aValue,
                                   std::index_sequence<I...>) noexcept
    {
        ((*(aDst + I) = byteAt<I>(aValue)), ...);
    }

    template <typename T, std::size_t... I>
    static constexpr void addNLeastSignificantBytesImpl(
        uint8_t *aDst, T &&aValue, std::index_sequence<I...>) noexcept
    {
        constexpr std::size_t kNBytes = sizeof(T) - sizeof...(I);
        ((*(aDst + I) = byteAt<kNBytes + I>(aValue)), ...);
    }

    template <typename T, std::size_t N>
    static constexpr void addNLeastSignificantBytes(uint8_t *aDst,
                                                    T &&aValue) noexcept
    {
        using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
        static_assert(N <= sizeof(UIntT), "Too big N.");
        using Indices = std::make_index_sequence<N>;
        addNLeastSignificantBytesImpl(aDst, std::forward<T>(aValue), Indices{});
    }

    template <typename T>
    static constexpr decltype(auto) highNBitsWithOffset(
        T &&aValue, uint_fast8_t aNBits, uint_fast8_t aOffset) noexcept
    {
        using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
        static_assert(is_uint_v<UIntT>, "UIntT must be unsigned integer type.");
        assert((aOffset + aNBits <= CHAR_BIT * sizeof(UIntT)) &&
               "UIntT cannot contain <aOffset + aNBits> bits");
        const auto kOffset = CHAR_BIT * sizeof(UIntT) - aNBits;
        const UIntT lowBitsErased = aValue >> kOffset;
        const UIntT result =
            static_cast<UIntT>(lowBitsErased << kOffset - aOffset);
        return result;
    }

    template <typename T>
    static constexpr decltype(auto) highNBits(T &&aValue,
                                              uint_fast8_t aNBits) noexcept
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
    static constexpr decltype(auto) lowNBits(T &&aValue,
                                             uint_fast8_t aNBits) noexcept
    {
        using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
        static_assert(is_uint_v<UIntT>, "UIntT must be unsigned integer type.");
        assert((aNBits <= CHAR_BIT * sizeof(UIntT)) && "Invalid aNBits");
        const auto kOffset = CHAR_BIT * sizeof(UIntT) - aNBits;
        const UIntT highBitsErased = static_cast<UIntT>(aValue << kOffset);
        const UIntT result = highBitsErased >> kOffset;
        return result;
    }

    template <typename T, std::size_t... I>
    static constexpr void get(uint8_t *aResult, uint8_t const *aSource,
                              std::index_sequence<I...>) noexcept
    {
        static_assert(
            is_uint_v<T>,
            "T must be unsigned integer type. And bool is forbidden.");
        static_assert(sizeof...(I) <= sizeof(T),
                      "Size of T not enough to contain I+1 bytes");
        ((*(aResult + CIndex<T, I>) = *(aSource + I)), ...);
    }

    template <typename T, std::size_t... I>
    static constexpr T get(uint8_t const *aSource,
                           std::index_sequence<I...> aSeq) noexcept
    {
        static_assert(
            is_uint_v<T>,
            "T must be unsigned integer type. And bool is forbidden.");
        static_assert(sizeof...(I) <= sizeof(T),
                      "Size of T not enough to contain I+1 bytes");
        T value;
        get<T>(reinterpret_cast<uint8_t *>(&value), aSource,
               std::forward<decltype(aSeq)>(aSeq));
        return value;
    }

    template <typename T, std::size_t... I1, std::size_t... I2>
    static constexpr T get(uint8_t const *aSource, std::index_sequence<I1...>,
                           std::index_sequence<I2...>) noexcept
    {
        static_assert(
            is_uint_v<T>,
            "T must be unsigned integer type. And bool is forbidden.");
        static_assert(sizeof...(I1) == sizeof...(I2),
                      "Index count does not match.");
        T value;
        ((*(reinterpret_cast<uint8_t *>(&value) + CIndex<T, I1>) =
              *(aSource + I2)),
         ...);
        return value;
    }

    template <typename T, std::size_t NBytes>
    static constexpr T valueFromLowBytes(uint8_t const *aSource) noexcept
    {
        static_assert(
            is_uint_v<T>,
            "T must be unsigned integer type. And bool is forbidden.");
        static_assert(NBytes <= sizeof(T),
                      "Size of T not enough to contain I+1 bytes");
        assert(aSource != nullptr && "Invalid aSource");
        using SrcIndices = std::make_index_sequence<NBytes>;
        using ValIndices = shifted_sequence_t<sizeof(T) - NBytes,
                                              std::make_index_sequence<NBytes>>;
        return get<T>(aSource, ValIndices{}, SrcIndices{});
    }

    template <typename T1, typename T2, typename T3>
    static constexpr decltype(auto) composition(T1 &&aDst, T2 &&aSrc,
                                                T3 &&aMask) noexcept
    {
        using UIntT = remove_cvref_t<T1>;
        static_assert(
            is_uint_v<UIntT>,
            "UIntT must be unsigned integer type. And bool is forbidden.");
        static_assert(std::is_same_v<UIntT, remove_cvref_t<T2>>, "Invalid T2");
        static_assert(std::is_same_v<UIntT, remove_cvref_t<T3>>, "Invalid T3");
        UIntT result = (aMask & aSrc) | (~aMask & aDst);
        return result;
    }

    template <typename T1, typename T2>
    static constexpr decltype(auto) addHighBits(T1 &&aTo, T2 &&aFrom,
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
    static constexpr decltype(auto) addLowBits(T1 &&aTo, T2 &&aFrom,
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
    static constexpr void addUInt64High(uint8_t *aDst,
                                        const uint_fast8_t aNBits,
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
        using Indices =
            shifted_sequence_t<1, std::make_index_sequence<sizeof(UIntT) - 1>>;
        addValue(aDst, std::forward<T>(aValue), Indices{});
    }

    static constexpr void addUInt64Low(uint8_t *aDst, uint_fast8_t aNBits,
                                       uint8_t aValue) noexcept
    {
        assert(aNBits < CHAR_BIT);
        assert(aNBits > 0);
        const uint8_t leftShiftedDst = static_cast<uint8_t>(*aDst << aNBits);
        const uint8_t dstVal = leftShiftedDst >> aNBits;
        const uint8_t srcVal =
            static_cast<uint8_t>(aValue << (CHAR_BIT - aNBits));
        *aDst = dstVal | srcVal;
    }

    template <typename To, typename From>
    static constexpr void copyLowest(To &&aToValue, From &&aFromValue,
                                     std::size_t aNBytes) noexcept
    {
        assert(aNBytes <= sizeof(To) && "aNBytes exceeds sizeof(To)");
        assert(aNBytes <= sizeof(From) && "aNBytes exceeds sizeof(From)");
        while (aNBytes > 0)
        {
            *bytePtrAt(aToValue, sizeof(To) - aNBytes) =
                *bytePtrAt(aFromValue, sizeof(From) - aNBytes);
            --aNBytes;
        }
    }

    template <typename T>
    static constexpr T get(uint8_t const *aData,
                           std::size_t aNBytesToRead) noexcept
    {
        static_assert(
            is_uint_v<T>,
            "T must be unsigned integer type. And bool is forbidden.");
        assert(aNBytesToRead <= sizeof(T) &&
               "T is not enough to contain aNBytesToRead bytes");
        T value;
        for (std::size_t i = 0; i < aNBytesToRead; ++i)
        {
            *(reinterpret_cast<uint8_t *>(&value) + Index<T>(i)) = *(aData + i);
        }
        return value;
    }

    template <typename T>
    static constexpr decltype(auto) nBitsWithOffset(
        T &&aValue, const uint_fast8_t aNBits,
        const uint_fast8_t aOffset) noexcept
    {
        using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
        assert(aOffset + aNBits <= sizeof(UIntT) * CHAR_BIT);
        assert(aNBits > 0);
        const UIntT kLAligned = aValue << sizeof(UIntT) * CHAR_BIT - aNBits;
        const UIntT kWithOffset = kLAligned >> aOffset;
        return kWithOffset;
    }

    template <typename T>
    static constexpr void add(uint8_t *aDst, T &&aValue) noexcept
    {
        using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
        using Indices = std::make_index_sequence<sizeof(UIntT)>;
        addValue(aDst, std::forward<T>(aValue), Indices{});
    }

    template <typename T>
    static constexpr decltype(auto) tuneOffset(T &&aValue,
                                               uint_fast8_t aDstOffset,
                                               uint_fast8_t aSrcOffset) noexcept
    {
        using UIntT = remove_cvref_t<T>;
        static_assert(is_uint_v<UIntT>,
                      "UIntT must be unsigned integer type and not bool.");
        assert(aDstOffset < sizeof(UIntT) * CHAR_BIT && "Invalid aDstOffset.");
        assert(aSrcOffset < sizeof(UIntT) * CHAR_BIT && "Invalid aSrcOffset.");
        aValue = (aDstOffset >= aSrcOffset)
                     ? aValue >> (aDstOffset - aSrcOffset)
                     : aValue << (aSrcOffset - aDstOffset);
        return std::forward<T>(aValue);
    }

    static constexpr std::size_t bytesCount(uint_fast8_t aOffset,
                                            std::size_t aNBits) noexcept
    {
        assert(aOffset < CHAR_BIT && "aOffset is too large.");
        if (aNBits > 0)
        {
            const std::size_t kAllBits = aOffset + aNBits;
            const std::size_t result =
                kAllBits / CHAR_BIT + ((kAllBits % CHAR_BIT != 0) ? 1 : 0);
            return result;
        }
        return 0;
    }

    template <typename T>
    static constexpr T getValue(uint8_t const *aData, uint_fast8_t aSrcOffset,
                                uint_fast8_t aNBits,
                                uint_fast8_t aDstOffset) noexcept
    {
        static_assert(is_uint_v<T>, "T must be unsigned int and not bool.");
        assert(aSrcOffset < CHAR_BIT && "aSrcOffset is too large.");
        assert(aDstOffset < CHAR_BIT && "aDstOffset is too large.");
        assert(aNBits <= sizeof(T) * CHAR_BIT && "aNBits is too large.");
        const uint_fast8_t kNSrcBits = aSrcOffset + aNBits;
        const uint_fast8_t kNDstBits = aDstOffset + aNBits;
        assert(kNDstBits <= sizeof(T) * CHAR_BIT &&
               "T is not able to contain kNDstBits bits.");
        assert(kNSrcBits <= sizeof(T) * CHAR_BIT &&
               "T is not able to contain kNSrcBits bits.");

        const std::size_t kNBytes = bytesCount(aSrcOffset, aNBits);
        T value = get<T>(aData, kNBytes);
        return tuneOffset(std::move(value), aDstOffset, aSrcOffset);
    }

    template <std::size_t NBytes, typename T>
    static constexpr void addBits(uint8_t *aDst, uint_fast8_t aOffset,
                                  std::size_t aNBits, T &&aValue) noexcept
    {
        static_assert(NBytes > 0, "NBytes must be positive");
        using UIntT = remove_cvref_t<T>;
        static_assert(is_uint_v<UIntT>, "UIntT must be unsigned integer");
        static_assert(NBytes <= sizeof(UIntT), "Invalid NBytes");
        using Indices = std::make_index_sequence<NBytes>;
        auto kMask = mask<UIntT>(aOffset, aNBits);
        UIntT result = composition(get<UIntT>(aDst, Indices{}),
                                   std::forward<T>(aValue), std::move(kMask));
        addValue(aDst, std::move(result), Indices{});
    }

    template <std::size_t NBytes>
    static constexpr void addBitsFromValue(uint8_t *aDst, uint_fast8_t aOffset,
                                           const uint64_t &aValue,
                                           std::size_t aNBits) noexcept
    {
        using FastUIntT = FastUInt<NBytes>;
        FastUIntT readyValue = static_cast<FastUIntT>(
            static_cast<FastUIntT>(aValue)
            << (sizeof(FastUIntT) * CHAR_BIT - aOffset - aNBits));
        addBits<NBytes>(aDst, aOffset, aNBits, std::move(readyValue));
    }

    using AddBitsPtr = void (*)(uint8_t *aDst, uint_fast8_t aOffset,
                                const uint64_t &aValue,
                                std::size_t aNBits) noexcept;
    using BitsAdderListT = std::array<AddBitsPtr, sizeof(uint64_t)>;

    template <std::size_t... I>
    static constexpr BitsAdderListT createBitsAdderListImpl(
        std::index_sequence<I...>) noexcept
    {
        return {(&Operation::addBitsFromValue<I>)...};
    }

    static constexpr BitsAdderListT createBitsAdderList() noexcept
    {
        using Indices = rabbit::shifted_sequence_t<
            1, std::make_index_sequence<sizeof(uint64_t)>>;
        return createBitsAdderListImpl(Indices{});
    }

    using ValueReaderPtr = uint64_t (*)(uint8_t const *aSrc) noexcept;
    using ValueReadersT = std::array<ValueReaderPtr, sizeof(uint64_t)>;

    template <std::size_t... I>
    static constexpr ValueReadersT createValueReadersImpl(
        std::index_sequence<I...>) noexcept
    {
        return {(&Operation::valueFromLowBytes<uint64_t, I>)...};
    }

    static constexpr ValueReadersT createValueReaders() noexcept
    {
        using Indices = rabbit::shifted_sequence_t<
            1, std::make_index_sequence<sizeof(uint64_t)>>;
        return createValueReadersImpl(Indices{});
    }
};
}  // namespace details
}  // namespace rabbit

#endif /* rabbit_details_h */