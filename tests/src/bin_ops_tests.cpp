#include <fmt/core.h>
#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <strong_type/strong_type.h>

#include <string_view>

#include "rabbit/endian.h"
#include "rabbit/utils.h"

namespace ndt
{
template <typename StrongT>
struct NecessaryOps
    : strong::plus<StrongT>
    , strong::plus_assignment<StrongT>
    , strong::minus<StrongT>
    , strong::minus_assignment<StrongT>
    , strong::convertible_to_bool<StrongT>
    , strong::modulo<StrongT>
    , strong::comparisons<StrongT>
{
};

using SrcBitOffset =
    strong::strong_type<struct SrcBitOffsetTag, uint_fast8_t, NecessaryOps>;
using DstBitOffset =
    strong::strong_type<struct DstBitOffsetTag, uint_fast8_t, NecessaryOps>;
using BitOffset =
    strong::strong_type<struct BitOffsetTag, uint_fast8_t, NecessaryOps>;
using NumBits =
    strong::strong_type<struct NumBitsTag, std::size_t, NecessaryOps>;

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
constexpr std::size_t CIndex = IndexImpl<kEndian>::get<T, I>();

template <typename T>
constexpr std::size_t Index(std::size_t I) noexcept
{
    return IndexImpl<kEndian>::get<T>(I);
}

class BinOpsImpl final
{
   public:
    template <typename T>
    static constexpr T mask(std::size_t aOffset, std::size_t aNBits) noexcept
    {
        static_assert(!std::is_volatile_v<T>, "T cannot be volatile");
        static_assert(!std::is_const_v<T>, "T cannot be const");
        static_assert(!std::is_reference_v<T>, "T cannot be reference.");
        static_assert(!std::is_pointer_v<T>, "T cannot be pointer");
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
        static_assert(!std::is_volatile_v<T>, "T cannot be volatile");
        static_assert(!std::is_const_v<T>, "T cannot be const");
        static_assert(!std::is_reference_v<T>, "T cannot be reference.");
        static_assert(!std::is_pointer_v<T>, "T cannot be pointer");
        static_assert(is_uint_v<T>, "T must be unsigned integer");
        assert((aOffset <= CHAR_BIT * sizeof(T)) && "Invalid aNBits.");
        constexpr T kSetBits = static_cast<T>(~T{});
        const T kMask = static_cast<T>(kSetBits >> aOffset);
        return kMask;
    }

    template <typename T>
    static constexpr T maskL(std::size_t aOffset) noexcept
    {
        static_assert(!std::is_volatile_v<T>, "T cannot be volatile");
        static_assert(!std::is_const_v<T>, "T cannot be const");
        static_assert(!std::is_reference_v<T>, "T cannot be reference.");
        static_assert(!std::is_pointer_v<T>, "T cannot be pointer");
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
        return {(&BinOpsImpl::addBitsFromValue<I>)...};
    }

    static constexpr BitsAdderListT createBitsAdderList() noexcept
    {
        using Indices =
            ndt::shifted_sequence_t<1,
                                    std::make_index_sequence<sizeof(uint64_t)>>;
        return createBitsAdderListImpl(Indices{});
    }

    using ValueReaderPtr = uint64_t (*)(uint8_t const *aSrc) noexcept;
    using ValueReadersT = std::array<ValueReaderPtr, sizeof(uint64_t)>;

    template <std::size_t... I>
    static constexpr ValueReadersT createValueReadersImpl(
        std::index_sequence<I...>) noexcept
    {
        return {(&BinOpsImpl::valueFromLowBytes<uint64_t, I>)...};
    }

    static constexpr ValueReadersT createValueReaders() noexcept
    {
        using Indices =
            ndt::shifted_sequence_t<1,
                                    std::make_index_sequence<sizeof(uint64_t)>>;
        return createValueReadersImpl(Indices{});
    }
};

class BinOps final
{
   public:
    template <typename T>
    static constexpr void add(uint8_t *aDst, T &&aValue) noexcept
    {
        using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
        static_assert(is_uint_v<UIntT>,
                      "UIntT must be unsigned integer type and not bool.");
        using Indices = std::make_index_sequence<sizeof(UIntT)>;
        BinOpsImpl::addValue(aDst, std::forward<T>(aValue), Indices{});
    }

    static constexpr void addBits(uint8_t *aDst, uint8_t const *aSrc,
                                  NumBits aNBits) noexcept
    {
        const std::size_t kWhole = aNBits.get() / CHAR_BIT;
        if (kWhole)
        {
            std::memcpy(aDst, aSrc, kWhole);
        }

        const uint_fast8_t kFraction = aNBits.get() % CHAR_BIT;
        if (kFraction)
        {
            *(aDst + kWhole) = BinOpsImpl::addHighBits(
                *(aDst + kWhole), *(aSrc + kWhole), kFraction);
        }
    }

    template <typename T>
    static constexpr void add(uint8_t *aDst, T &&aValue,
                              uint_fast8_t aNBits) noexcept
    {
        using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
        static_assert(is_uint_v<UIntT>,
                      "UIntT must be unsigned integer type and not bool.");

        UIntT lAlignedValue =
            static_cast<UIntT>(aValue << (sizeof(UIntT) * CHAR_BIT - aNBits));
        const UIntT readyValue = toNet(std::move(lAlignedValue));
        addBits(aDst, reinterpret_cast<uint8_t const *>(&readyValue),
                NumBits(aNBits));
    }

    template <typename T>
    static constexpr void add(uint8_t *aDst, uint_fast8_t aOffset, T &&aValue,
                              uint_fast8_t aNBits) noexcept
    {
        using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
        static_assert(is_uint_v<UIntT>,
                      "UIntT must be unsigned integer type and not bool.");

        assert(aDst != nullptr && "Invalid aDst");
        assert(aOffset < CHAR_BIT && "Invalid aOffset");
        assert(aNBits <= sizeof(UIntT) * CHAR_BIT && "Invalid aNBits");
        if (aNBits == 0)
        {
            return;
        }

        if (aOffset + aNBits <= sizeof(uint64_t) * CHAR_BIT)
        {
            //            const UIntT leftAligned = aValue <<
            //            (sizeof(UIntT)*CHAR_BIT - aNBits); const UIntT
            //            netLeftAligned = toNet(leftAligned); add(aDst,
            //            aOffset, reinterpret_cast<uint8_t const
            //            *>(&netLeftAligned), 0, aNBits);
            if (aOffset == 0)
            {
                add(aDst, std::forward<T>(aValue), aNBits);
            }
            else
            {
                const auto kNBytes = BinOpsImpl::bytesCount(aOffset, aNBits);
                assert(kNBytes > 0 && "Invalid kNBytes.");
                assert(kNBytes <= std::tuple_size_v<decltype(bitAdders_)> &&
                       "Invalid kNBytes.");

                if constexpr (std::is_same_v<UIntT, uint64_t>)
                {
                    bitAdders_[kNBytes - 1](aDst, aOffset, aValue, aNBits);
                }
                else
                {
                    uint64_t valueCopy = aValue;
                    bitAdders_[kNBytes - 1](aDst, aOffset, valueCopy, aNBits);
                }
            }
        }
        else
        {
            const uint_fast8_t kRest = sizeof(uint64_t) * CHAR_BIT - aNBits;
            const uint64_t noLeadingZeros =
                static_cast<uint64_t>(aValue << kRest);
            const uint64_t shiftedVal = noLeadingZeros >> aOffset;
            BinOpsImpl::addUInt64High(aDst,
                                      sizeof(uint64_t) * CHAR_BIT - aOffset,
                                      std::move(shiftedVal));

            const uint_fast8_t kOverlap = aOffset - kRest;
            BinOpsImpl::addUInt64Low(aDst + sizeof(uint64_t), kOverlap,
                                     BinOpsImpl::byteAt<sizeof(uint64_t) - 1>(
                                         std::forward<T>(aValue)));
        }
    }

    static constexpr void addBits(uint8_t *aDst, DstBitOffset aDstOffset,
                                  uint8_t const *aSrc, SrcBitOffset aSrcOffset,
                                  NumBits aNBits) noexcept
    {
        if (aDstOffset)
        {
            const uint_fast8_t kDstBitsToByteBorder =
                CHAR_BIT - aDstOffset.get();
            const uint_fast8_t kBitsToAdd = std::min(
                kDstBitsToByteBorder, static_cast<uint_fast8_t>(aNBits.get()));
            const uint_fast8_t kSrcNBits = aSrcOffset.get() + kBitsToAdd;

            const uint_fast8_t kNBytesToRead = kSrcNBits > CHAR_BIT ? 2 : 1;
            constexpr std::size_t kNBytesToWrite = 1;

            uint64_t value = valueReaders_[kNBytesToRead - 1](aSrc) >>
                             (kNBytesToRead * CHAR_BIT - kSrcNBits);
            BinOpsImpl::addBitsFromValue<kNBytesToWrite>(aDst, aDstOffset.get(),
                                                         value, kBitsToAdd);

            aNBits -= kBitsToAdd;

            if (!aNBits)
            {
                return;
            }

            aSrc += kSrcNBits / CHAR_BIT;
            aDst += 1;

            aSrcOffset = SrcBitOffset(kSrcNBits % CHAR_BIT);
        }

        constexpr uint_fast8_t kMaxBitsPerAction =
            (sizeof(uint64_t) - 1) * CHAR_BIT;

        const std::size_t loopCount = aNBits.get() / kMaxBitsPerAction;
        uint64_t value{};
        for (std::size_t i = 0; i < loopCount; ++i)
        {
            constexpr uint_fast8_t kNBytesToRead = sizeof(uint64_t);
            constexpr uint_fast8_t kNBytesToWrite = kNBytesToRead - 1;
            using ReadIndices = std::make_index_sequence<kNBytesToRead>;
            BinOpsImpl::get<uint64_t>(reinterpret_cast<uint8_t *>(&value), aSrc,
                                      ReadIndices{});

            value <<= aSrcOffset.get();

            using WriteIndices = std::make_index_sequence<kNBytesToWrite>;
            BinOpsImpl::addValue(aDst, value, WriteIndices{});

            aSrc += kNBytesToWrite;
            aDst += kNBytesToWrite;
        }

        const auto kRestBits = aNBits % kMaxBitsPerAction;
        if (kRestBits)
        {
            std::size_t kNBytesToRead =
                ndt::BinOpsImpl::bytesCount(aSrcOffset.get(), kRestBits.get());
            value =
                valueReaders_[kNBytesToRead - 1](aSrc) >>
                (kNBytesToRead * CHAR_BIT - aSrcOffset.get() - kRestBits.get());

            std::size_t kNBytesToWrite =
                ndt::BinOpsImpl::bytesCount(0, kRestBits.get());
            bitAdders_[kNBytesToWrite - 1](aDst, 0, value, kRestBits.get());
        }
    }

    static constexpr void addBits(uint8_t *aDst, uint8_t const *aSrc,
                                  BitOffset aOffset, NumBits aNBits) noexcept
    {
        assert(aDst != nullptr && "Invalid aDst");
        assert(aSrc != nullptr && "Invalid aSrc");
        assert(aOffset < CHAR_BIT && "Invalid aOffset");
        std::size_t byteOffset = 0;
        if (aOffset)
        {
            const uint_fast8_t kBitsToByteBorder = CHAR_BIT - aOffset.get();
            if (aNBits >= kBitsToByteBorder)
            {
                *aDst = BinOpsImpl::addLowBits(*aDst, *aSrc, kBitsToByteBorder);
                ++byteOffset;
                aNBits -= NumBits(kBitsToByteBorder);
            }
            else
            {
                BinOpsImpl::addBits<1>(aDst, aOffset.get(), aNBits.get(),
                                       *aSrc);
                return;
            }
        }
        addBits(aDst + byteOffset, aSrc + byteOffset, aNBits);
    }

    static constexpr void add(uint8_t *aDst, DstBitOffset aDstOffset,
                              uint8_t const *aSrc, SrcBitOffset aSrcOffset,
                              NumBits aNBits) noexcept
    {
        assert(aDst != nullptr && "Invalid aDst");
        assert(aSrc != nullptr && "Invalid aSrc");
        assert(aDstOffset < CHAR_BIT && "Invalid aDstOffset");
        assert(aSrcOffset < CHAR_BIT && "Invalid aSrcOffset");
        if (!aNBits)
        {
            return;
        }

        if (aDstOffset == aSrcOffset)
        {
            addBits(aDst, aSrc, BitOffset(aDstOffset.get()), aNBits);
        }
        else
        {
            addBits(aDst, aDstOffset, aSrc, aSrcOffset, aNBits);
        }
    }

   private:
    static constexpr auto bitAdders_ = BinOpsImpl::createBitsAdderList();
    static constexpr auto valueReaders_ = BinOpsImpl::createValueReaders();
};

}  // namespace ndt

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
        return ndt::concatenate_arrays(ndt::make_array(aArr1[I1]...),
                                       ndt::make_array(aArr2[I2]...));
    }
    else
    {
        return ndt::make_array(aArr1[I1]...);
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
    return ndt::concatenate_arrays(
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
    void print() { fmt::print("{}\n", to_string()); }

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

TEST_F(N2BinOpsTest, ReaderConstructor)
{
    ASSERT_EQ(formatStr_.size(), 13);
    rawData_[0] = 0b00001111;
    rawData_[1] = 0b00000101;
}

TEST(BinOpsTest, Mask1)
{
    constexpr auto kMask = ndt::BinOpsImpl::mask<uint_fast8_t>(2, 3);
    constexpr uint_fast8_t expectedMask{0b00111000};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, InvertedMask1)
{
    constexpr auto kMask = ndt::BinOpsImpl::invertedMask<uint_fast8_t>(2, 3);
    constexpr uint_fast8_t expectedMask{0b11000111};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, Mask2)
{
    constexpr auto kMask = ndt::BinOpsImpl::mask<uint_fast16_t>(6, 5);
    constexpr uint_fast16_t expectedMask{0b0000001111100000};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, InvertedMask2)
{
    constexpr auto kMask = ndt::BinOpsImpl::invertedMask<uint_fast16_t>(6, 5);
    constexpr uint_fast16_t expectedMask{0b1111110000011111};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, Mask3)
{
    constexpr auto kMask = ndt::BinOpsImpl::mask<uint_fast32_t>(7, 10);
    constexpr uint_fast32_t expectedMask{0b00000001111111111000000000000000};
    ASSERT_EQ(kMask, expectedMask);
}

TEST(BinOpsTest, InvertedMask3)
{
    constexpr auto kMask = ndt::BinOpsImpl::invertedMask<uint_fast32_t>(7, 10);
    constexpr uint_fast32_t expectedMask{0b11111110000000000111111111111111};
    ASSERT_EQ(kMask, expectedMask);
}

TEST_F(N2BinOpsTest, Add1BitAt0Offset)
{
    constexpr uint16_t value = 1;
    constexpr uint_fast8_t kNBits = 1;
    constexpr uint_fast8_t kOffset = 0;
    ndt::BinOps::add(rawData_.data(), kOffset, value, kNBits);
    ASSERT_EQ(rawData_[0], 0b10000000);
    ASSERT_EQ(rawData_[1], 0b00000000);
}

TEST_F(N2BinOpsTest, Add3BitsAt0Offset)
{
    constexpr uint16_t value = 5;
    constexpr uint_fast8_t kNBits = 3;
    constexpr uint_fast8_t kOffset = 0;
    ndt::BinOps::add(rawData_.data(), kOffset, value, kNBits);
    ASSERT_EQ(rawData_[0], 0b10100000);
    ASSERT_EQ(rawData_[1], 0b00000000);
}

TEST_F(N2BinOpsTest, Add8BitsAt2Offset)
{
    rawData_[0] = 0b10000000;
    rawData_[1] = 0b00010010;

    constexpr uint16_t value = 0b11111111;
    constexpr uint_fast8_t kNBits = 8;
    constexpr uint_fast8_t kOffset = 2;

    ndt::BinOps::add(rawData_.data(), kOffset, value, kNBits);
    ASSERT_EQ(rawData_[0], 0b10111111);
    ASSERT_EQ(rawData_[1], 0b11010010);
}

TEST_F(N2BinOpsTest, Add11BitsAt1Offset)
{
    rawData_[0] = 0b10011100;
    rawData_[1] = 0b01100110;

    constexpr uint16_t value = 0b0000011101111111;
    constexpr uint_fast8_t kNBits = 11;
    constexpr uint_fast8_t kOffset = 1;

    ndt::BinOps::add(rawData_.data(), kOffset, value, kNBits);
    ASSERT_EQ(rawData_[0], 0b11110111);
    ASSERT_EQ(rawData_[1], 0b11110110);
}

TEST(BinOpsTest, BytePtrAtPointerToMostSignificantByte)
{
    const uint32_t value = 0;
    uint8_t const *MSBPtr = nullptr;
    auto ptr = ndt::BinOpsImpl::bytePtrAt<0>(value);
    if constexpr (ndt::kEndian == ndt::eEndian::kLittle)
    {
        MSBPtr = reinterpret_cast<uint8_t const *>(&value) + sizeof(value) - 1;
    }
    else if constexpr (ndt::kEndian == ndt::eEndian::kBig)
    {
        MSBPtr = reinterpret_cast<uint8_t const *>(&value);
    }
    ASSERT_EQ(ptr, MSBPtr);
}

TEST(BinOpsTest, BytePtrAtPointerToLeastSignificantByte)
{
    const uint32_t value = 0;
    uint8_t const *MSBPtr = nullptr;
    auto ptr = ndt::BinOpsImpl::bytePtrAt<sizeof(value) - 1>(value);
    if constexpr (ndt::kEndian == ndt::eEndian::kLittle)
    {
        MSBPtr = reinterpret_cast<uint8_t const *>(&value);
    }
    else if constexpr (ndt::kEndian == ndt::eEndian::kBig)
    {
        MSBPtr = reinterpret_cast<uint8_t const *>(&value) + sizeof(value) - 1;
    }
    ASSERT_EQ(ptr, MSBPtr);
}

TEST(BinOpsTest, ByteAtUInt32t)
{
    uint32_t value = 0b00001011'00010110'00100001'00101100;
    ASSERT_EQ(ndt::BinOpsImpl::byteAt<0>(value), 0b00001011);
    ASSERT_EQ(ndt::BinOpsImpl::byteAt<1>(value), 0b00010110);
    ASSERT_EQ(ndt::BinOpsImpl::byteAt<2>(value), 0b00100001);
    ASSERT_EQ(ndt::BinOpsImpl::byteAt<3>(value), 0b00101100);
}

template <typename T>
constexpr decltype(auto) check_highNBits(T &&aValue,
                                         uint_fast8_t aNBits) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(ndt::is_uint_v<UIntT>,
                  "UIntT must be unsigned integer and not bool.");
    auto result = ndt::BinOpsImpl::highNBits(std::forward<T>(aValue), aNBits);
    static_assert(std::is_same_v<decltype(result), UIntT>,
                  "Deduced type is invalid.");
    return result;
}

template <typename T>
constexpr decltype(auto) check_lowNBits(T &&aValue,
                                        uint_fast8_t aNBits) noexcept
{
    using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(ndt::is_uint_v<UIntT>,
                  "UIntT must be unsigned integer and not bool.");
    auto result = ndt::BinOpsImpl::lowNBits(std::forward<T>(aValue), aNBits);
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
    ndt::BinOpsImpl::addValue(rawData_.data(), valueToAdd, Indices{});
    ASSERT_EQ(rawData_[0], valueToAdd);
}

TEST_F(N2BinOpsTest, addValue8)
{
    using U = uint8_t;
    using Indices = std::make_index_sequence<sizeof(U)>;
    constexpr U valueToAdd{172};
    constexpr uint8_t secondValue{0b11110111};
    rawData_[1] = secondValue;
    ndt::BinOpsImpl::addValue(rawData_.data(), valueToAdd, Indices{});
    ASSERT_EQ(rawData_[0], valueToAdd);
    ASSERT_EQ(rawData_[1], secondValue);
}

TEST_F(N2BinOpsTest, addValue16)
{
    using U = uint16_t;
    using Indices = std::make_index_sequence<sizeof(U)>;
    constexpr U valueToAdd{0b00010000'11111111};
    ndt::BinOpsImpl::addValue(rawData_.data(), valueToAdd, Indices{});
    ASSERT_EQ(rawData_[0], uint8_t{0b00010000});
    ASSERT_EQ(rawData_[1], uint8_t{0b11111111});
}

TEST_F(N4BinOpsTest, addValue32)
{
    using U = uint32_t;
    using Indices = std::make_index_sequence<sizeof(U)>;
    constexpr U valueToAdd{0b10101010'10100101'00010000'11111111};
    ndt::BinOpsImpl::addValue(rawData_.data(), valueToAdd, Indices{});
    ASSERT_EQ(rawData_[0], uint8_t{0b10101010});
    ASSERT_EQ(rawData_[1], uint8_t{0b10100101});
    ASSERT_EQ(rawData_[2], uint8_t{0b00010000});
    ASSERT_EQ(rawData_[3], uint8_t{0b11111111});
}

TEST_F(N4BinOpsTest, addValue32Shifted)
{
    using U = uint32_t;
    using Indices =
        ndt::shifted_sequence_t<1, std::make_index_sequence<sizeof(U) - 1>>;
    constexpr U valueToAdd{0b10101010'10100101'00010000'11111111};
    ndt::BinOpsImpl::addValue(rawData_.data(), valueToAdd, Indices{});
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
    ndt::BinOpsImpl::addValue(rawData_.data(), valueToAdd, Indices{});
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
    ndt::BinOpsImpl::addUInt64High(rawData_.data(), 59, valueToAdd);
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
    ndt::BinOpsImpl::addUInt64Low(rawData_.data(), 5, valueToAdd);
    ASSERT_EQ(rawData_[0], uint8_t{0b11101000});
}

TEST(BinOpsTest, nBitsWithOffset)
{
    using U = uint32_t;
    constexpr U valueToAdd{0b10101010'11100101'00010000'11110111};
    constexpr U result = ndt::BinOpsImpl::nBitsWithOffset(valueToAdd, 19, 3);
    ASSERT_EQ(result, U{0b00010100'01000011'11011100'00000000});
}

TEST_F(N1BinOpsTest, add4BitsAt2Offset)
{
    using U = uint8_t;
    constexpr std::size_t kMaxBytes = sizeof(U);
    using UIntT = ndt::FastUInt<kMaxBytes>;
    constexpr UIntT valueToAdd{0b01001011};
    constexpr uint_fast8_t kNBits = 4;
    constexpr uint_fast8_t kOffset = 2;
    // ndt::BinOpsImpl::add<kMaxBytes>(rawData_.data(), 2, valueToAdd, 4);
    ndt::BinOps::add(rawData_.data(), kOffset, valueToAdd, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00101100});
}

TEST_F(N1BinOpsTest, add5BitsAt0Offset)
{
    using U = uint8_t;
    constexpr std::size_t kMaxBytes = sizeof(U);
    using UIntT = ndt::FastUInt<kMaxBytes>;
    UIntT valueToAdd{0b01001011};
    constexpr uint_fast8_t kNBits = 5;
    constexpr uint_fast8_t kOffset = 0;
    rawData_[0] = 0b01001110;
    ndt::BinOps::add(rawData_.data(), kOffset, valueToAdd, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b01011110});
}

TEST_F(N2BinOpsTest, add4BitsAt5Offset)
{
    using U = uint8_t;
    constexpr std::size_t kMaxBytes = sizeof(U) + 1;
    using UIntT = ndt::FastUInt<kMaxBytes>;
    constexpr UIntT valueToAdd{0b01001011};
    constexpr uint_fast8_t kNBits = 4;
    constexpr uint_fast8_t kOffset = 5;
    rawData_[0] = 0b10111111;
    rawData_[1] = 0b11110111;
    ndt::BinOps::add(rawData_.data(), kOffset, valueToAdd, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10111101});
    ASSERT_EQ(rawData_[1], uint8_t{0b11110111});
}

TEST_F(N3BinOpsTest, add15BitsAt6Offset)
{
    using U = uint16_t;
    constexpr std::size_t kMaxBytes = sizeof(U) + 1;
    using UIntT = ndt::FastUInt<kMaxBytes>;
    constexpr UIntT valueToAdd{0b01001011'00111101};
    constexpr uint_fast8_t kNBits = 15;
    constexpr uint_fast8_t kOffset = 6;
    rawData_[0] = 0b10111111;
    rawData_[1] = 0b11110111;
    rawData_[2] = 0b11110101;
    ndt::BinOps::add(rawData_.data(), kOffset, valueToAdd, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10111110});
    ASSERT_EQ(rawData_[1], uint8_t{0b01011001});
    ASSERT_EQ(rawData_[2], uint8_t{0b11101101});
}

TEST_F(N3BinOpsTest, add16BitsAt5Offset)
{
    using U = uint16_t;
    constexpr std::size_t kMaxBytes = sizeof(U) + 1;
    using UIntT = ndt::FastUInt<kMaxBytes>;
    constexpr UIntT valueToAdd{0b01001011'00111101};
    constexpr uint_fast8_t kNBits = 16;
    constexpr uint_fast8_t kOffset = 5;
    rawData_[0] = 0b10111111;
    rawData_[1] = 0b11110111;
    rawData_[2] = 0b11110101;
    ndt::BinOps::add(rawData_.data(), kOffset, valueToAdd, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10111010});
    ASSERT_EQ(rawData_[1], uint8_t{0b01011001});
    ASSERT_EQ(rawData_[2], uint8_t{0b11101101});
}

TEST_F(N8BinOpsTest, add30BitsAt5Offset)
{
    using U = uint32_t;
    constexpr std::size_t kMaxBytes = sizeof(U) + 1;
    using UIntT = ndt::FastUInt<kMaxBytes>;
    constexpr UIntT valueToAdd{0b01001011'00111101'01010101'11100011};
    constexpr uint_fast8_t kNBits = 30;
    constexpr uint_fast8_t kOffset = 5;
    rawData_[0] = 0b10111111;
    rawData_[1] = 0b11110111;
    rawData_[2] = 0b11110101;
    rawData_[3] = 0b11110101;
    rawData_[4] = 0b11110101;
    ndt::BinOps::add(rawData_.data(), kOffset, valueToAdd, kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10111001});
    ASSERT_EQ(rawData_[1], uint8_t{0b01100111});
    ASSERT_EQ(rawData_[2], uint8_t{0b10101010});
    ASSERT_EQ(rawData_[3], uint8_t{0b10111100});
    ASSERT_EQ(rawData_[4], uint8_t{0b01110101});
    ASSERT_EQ(rawData_[5], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[6], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[7], uint8_t{0b00000000});
}

TEST_F(N8BinOpsTest, add43BitsAt6Offset)
{
    using U = uint64_t;
    constexpr std::size_t kMaxBytes = sizeof(U);
    using UIntT = ndt::FastUInt<kMaxBytes>;
    constexpr UIntT valueToAdd{
        0b01001011'00111101'01010101'11100011'11100011'11100011'11100011'11101011};
    constexpr uint_fast8_t kNBits = 43;
    constexpr uint_fast8_t kOffset = 6;
    rawData_[0] = 0b10111111;
    rawData_[1] = 0b11110111;
    rawData_[2] = 0b11110101;
    rawData_[3] = 0b11110101;
    rawData_[4] = 0b11110101;
    ndt::BinOps::add(rawData_.data(), kOffset, valueToAdd, kNBits);
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
    ndt::BinOpsImpl::add(rawData_.data(), kValue);
    ASSERT_EQ(rawData_[0], kValue);
}

TEST_F(N2BinOpsTest, addUInt8WithoutOffset)
{
    using U = uint8_t;
    constexpr U kValue = 0b10101111;
    ndt::BinOpsImpl::add(rawData_.data(), kValue);
    ASSERT_EQ(rawData_[0], kValue);
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, addUInt16WithoutOffset)
{
    using U = uint16_t;
    constexpr U kValue = 0b10101111'00010111;
    ndt::BinOpsImpl::add(rawData_.data(), kValue);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101111});
    ASSERT_EQ(rawData_[1], uint8_t{0b00010111});
}

TEST_F(N3BinOpsTest, addUInt16WithoutOffset)
{
    using U = uint16_t;
    constexpr U kValue = 0b10101111'00010111;
    ndt::BinOpsImpl::add(rawData_.data() + 1, kValue);
    ASSERT_EQ(rawData_[0], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[1], uint8_t{0b10101111});
    ASSERT_EQ(rawData_[2], uint8_t{0b00010111});
}

TEST_F(N4BinOpsTest, addUInt32WithoutOffset)
{
    using U = uint32_t;
    constexpr U kValue = 0b10101111'00010111'00001111'00110011;
    ndt::BinOpsImpl::add(rawData_.data(), kValue);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101111});
    ASSERT_EQ(rawData_[1], uint8_t{0b00010111});
    ASSERT_EQ(rawData_[2], uint8_t{0b00001111});
    ASSERT_EQ(rawData_[3], uint8_t{0b00110011});
}

TEST_F(N8BinOpsTest, addUInt32WithoutOffset)
{
    using U = uint32_t;
    constexpr U kValue = 0b10101111'00010111'00001111'00110011;
    ndt::BinOpsImpl::add(rawData_.data() + 2, kValue);
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
    ndt::BinOpsImpl::add(rawData_.data(), kValue);
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
    ndt::BinOpsImpl::addNLeastSignificantBytes<const U &&, 3>(
        rawData_.data(), std::move(kValue));
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
    ndt::BinOpsImpl::addNLeastSignificantBytes<const U &&, 1>(
        rawData_.data(), std::move(kValue));
    ASSERT_EQ(rawData_[0], uint8_t{0b10101111});
}

TEST_F(N4BinOpsTest, AddZeroLeastSignificantBytes)
{
    using U = uint64_t;
    constexpr U kValue =
        0b10101111'00010111'00001111'00110011'10101010'01111110'10000001'10011001;
    ndt::BinOpsImpl::addNLeastSignificantBytes<const U &&, 0>(
        rawData_.data(), std::move(kValue));
    ASSERT_EQ(rawData_[0], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[2], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[3], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, Add0BitsAt0Offset)
{
    std::array<uint8_t, 2> source{0b10101111, 0b10101111};
    constexpr ndt::DstBitOffset kDstOffset{0};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{0};
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00000000});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, Add1BitsAt0Offset)
{
    std::array<uint8_t, 2> source{0b10101111, 0b01111110};
    constexpr ndt::DstBitOffset kDstOffset{0};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{1};
    rawData_[0] = 0b00110011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10110011});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, Add2BitsAt0Offset)
{
    std::array<uint8_t, 2> source{0b11101111, 0b01111110};
    constexpr ndt::DstBitOffset kDstOffset{0};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{2};
    rawData_[0] = 0b00110011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b11110011});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, Add5BitsAt0Offset)
{
    std::array<uint8_t, 2> source{0b11101111, 0b01111110};
    constexpr ndt::DstBitOffset kDstOffset{0};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{5};
    rawData_[0] = 0b00110011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b11101011});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, Add8BitsAt0Offset)
{
    std::array<uint8_t, 2> source{0b10011001, 0b01111110};
    constexpr ndt::DstBitOffset kDstOffset{0};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{CHAR_BIT};
    rawData_[0] = 0b00110011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10011001});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, Add9BitsAt0Offset)
{
    std::array<uint8_t, 2> source{0b10011001, 0b10101010};
    constexpr ndt::DstBitOffset kDstOffset{0};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{9};
    rawData_[0] = 0b00110011;
    rawData_[1] = 0b00110011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10011001});
    ASSERT_EQ(rawData_[1], uint8_t{0b10110011});
}

TEST_F(N2BinOpsTest, Add15BitsAt0Offset)
{
    std::array<uint8_t, 2> source{0b10011001, 0b10101010};
    constexpr ndt::DstBitOffset kDstOffset{0};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{15};
    rawData_[0] = 0b00110011;
    rawData_[1] = 0b00110011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10011001});
    ASSERT_EQ(rawData_[1], uint8_t{0b10101011});
}

TEST_F(N2BinOpsTest, Add16BitsAt0Offset)
{
    std::array<uint8_t, 2> source{0b10011001, 0b10101010};
    constexpr ndt::DstBitOffset kDstOffset{0};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{16};
    rawData_[0] = 0b00110011;
    rawData_[1] = 0b00110011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
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
        ndt::BinOpsImpl::highNBitsWithOffset(kValue, kNBits, kDstOffset);
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
        ndt::BinOpsImpl::highNBitsWithOffset(kValue, kNBits, kDstOffset);
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
        ndt::BinOpsImpl::highNBitsWithOffset(kValue, kNBits, kDstOffset);
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
        ndt::BinOpsImpl::highNBitsWithOffset(kValue, kNBits, kDstOffset);
    static_assert(std::is_same_v<std::remove_const_t<decltype(high4Bits)>, U>,
                  "high4Bits is of invalid type");
    ASSERT_EQ(high4Bits, U{0b00000000'00001001});
}

TEST_F(N2BinOpsTest, Add1BitsAt1Offset)
{
    std::array<uint8_t, 2> source{0b10101111, 0b01111110};
    constexpr ndt::DstBitOffset kDstOffset{1};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{1};
    rawData_[0] = 0b00110011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b01110011});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, Add2BitsAt1Offset)
{
    std::array<uint8_t, 2> source{0b10101111, 0b01111110};
    constexpr ndt::DstBitOffset kDstOffset{1};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{2};
    rawData_[0] = 0b00110011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b01010011});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, Add7BitsAt1Offset)
{
    std::array<uint8_t, 2> source{0b10101101, 0b01111110};
    constexpr ndt::DstBitOffset kDstOffset{1};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{7};
    rawData_[0] = 0b00110011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b01010110});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, Add0BitsAt1Offset)
{
    std::array<uint8_t, 2> source{0b10101101, 0b01111110};
    constexpr ndt::DstBitOffset kDstOffset{1};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{0};
    rawData_[0] = 0b00110011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00110011});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000000});
}

TEST_F(N2BinOpsTest, Add8BitsAt1Offset)
{
    std::array<uint8_t, 2> source{0b10101101, 0b01111110};
    constexpr ndt::DstBitOffset kDstOffset{1};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{8};
    rawData_[0] = 0b00110011;
    rawData_[1] = 0b00100011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b01010110});
    ASSERT_EQ(rawData_[1], uint8_t{0b10100011});
}

TEST_F(N2BinOpsTest, Add10BitsAt3Offset)
{
    std::array<uint8_t, 2> source{0b10101101, 0b01111110};
    constexpr ndt::DstBitOffset kDstOffset{3};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{10};
    rawData_[0] = 0b00110011;
    rawData_[1] = 0b00100011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00110101});
    ASSERT_EQ(rawData_[1], uint8_t{0b10101011});
}

TEST_F(N2BinOpsTest, Add13BitsAt3Offset)
{
    std::array<uint8_t, 2> source{0b10101101, 0b01111110};
    constexpr ndt::DstBitOffset kDstOffset{3};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{13};
    rawData_[0] = 0b00110011;
    rawData_[1] = 0b00100010;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00110101});
    ASSERT_EQ(rawData_[1], uint8_t{0b10101111});
}

TEST_F(N3BinOpsTest, Add13BitsAt5Offset)
{
    std::array<uint8_t, 2> source{0b10101101, 0b01111110};
    constexpr ndt::DstBitOffset kDstOffset{5};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{13};
    rawData_[0] = 0b11011011;
    rawData_[1] = 0b00100010;
    rawData_[2] = 0b00100011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b11011101});
    ASSERT_EQ(rawData_[1], uint8_t{0b01101011});
    ASSERT_EQ(rawData_[2], uint8_t{0b11100011});
}

TEST_F(N4BinOpsTest, Add23BitsAt7Offset)
{
    std::array<uint8_t, 4> source{0b00001111, 0b00110011, 0b10101010,
                                  0b01111110};
    constexpr ndt::DstBitOffset kDstOffset{7};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{23};
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101110});
    ASSERT_EQ(rawData_[1], uint8_t{0b00011110});
    ASSERT_EQ(rawData_[2], uint8_t{0b01100111});
    ASSERT_EQ(rawData_[3], uint8_t{0b01010100});
}

TEST_F(N8BinOpsTest, Add35BitsAt5Offset)
{
    std::array<uint8_t, 8> source{0b10101111, 0b00010111, 0b00001111,
                                  0b00110011, 0b10101010, 0b01111110,
                                  0b10000001, 0b10011001};
    constexpr ndt::DstBitOffset kDstOffset{5};
    constexpr ndt::SrcBitOffset kSrcOffset{0};
    constexpr ndt::NumBits kNBits{35};
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    rawData_[4] = 0b11000011;
    rawData_[5] = 0b11100111;
    rawData_[6] = 0b10111101;
    rawData_[7] = 0b00111100;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101101});
    ASSERT_EQ(rawData_[1], uint8_t{0b01111000});
    ASSERT_EQ(rawData_[2], uint8_t{0b10111000});
    ASSERT_EQ(rawData_[3], uint8_t{0b01111001});
    ASSERT_EQ(rawData_[4], uint8_t{0b10011101});
    ASSERT_EQ(rawData_[5], uint8_t{0b11100111});
    ASSERT_EQ(rawData_[6], uint8_t{0b10111101});
    ASSERT_EQ(rawData_[7], uint8_t{0b00111100});
}

TEST_F(N1BinOpsTest, Add3BitsFrom5To2Offset)
{
    std::array<uint8_t, 1> source{0b10101101};
    constexpr ndt::DstBitOffset kDstOffset{2};
    constexpr ndt::SrcBitOffset kSrcOffset{5};
    constexpr ndt::NumBits kNBits{3};
    rawData_[0] = 0b10110011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101011});
}

TEST_F(N1BinOpsTest, Add4BitsFrom1To3Offset)
{
    std::array<uint8_t, 1> source{0b01101000};
    constexpr ndt::DstBitOffset kDstOffset{3};
    constexpr ndt::SrcBitOffset kSrcOffset{1};
    constexpr ndt::NumBits kNBits{4};
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00011010});
}

TEST_F(N2BinOpsTest, Add5BitsFrom2To4Offset)
{
    std::array<uint8_t, 1> source{0b01100110};
    constexpr ndt::DstBitOffset kDstOffset{4};
    constexpr ndt::SrcBitOffset kSrcOffset{2};
    constexpr ndt::NumBits kNBits{5};
    rawData_[0] = 0b00110011;
    rawData_[1] = 0b00100010;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b00111001});
    ASSERT_EQ(rawData_[1], uint8_t{0b10100010});
}

TEST_F(N2BinOpsTest, Add10BitsFrom10To5Offset)
{
    std::array<uint8_t, 3> source{0b01100110, 0b10011001};
    constexpr ndt::DstBitOffset kDstOffset{5};
    constexpr ndt::SrcBitOffset kSrcOffset{2};
    constexpr ndt::NumBits kNBits{10};
    rawData_[0] = 0b00111100;
    rawData_[1] = 0b11000011;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
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
    const auto value = ndt::BinOpsImpl::get<uint16_t>(rawData_.data(), kNBytes);
    static_assert(
        std::is_same_v<std::remove_const_t<decltype(value)>, uint16_t>,
        "value is of invalid type.");
    ASSERT_EQ(ndt::BinOpsImpl::byteAt<0>(value), uint8_t{0b10101111});
}

TEST_F(N4BinOpsTest, GetUInt16From2Bytes)
{
    constexpr std::size_t kNBytes = 2;
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    const auto value = ndt::BinOpsImpl::get<uint16_t>(rawData_.data(), kNBytes);
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
    const auto value = ndt::BinOpsImpl::get<uint32_t>(rawData_.data(), kNBytes);
    static_assert(
        std::is_same_v<std::remove_const_t<decltype(value)>, uint32_t>,
        "value is of invalid type.");
    ASSERT_EQ(ndt::BinOpsImpl::byteAt<0>(value), uint8_t{0b10101111});
    ASSERT_EQ(ndt::BinOpsImpl::byteAt<1>(value), uint8_t{0b10000001});
    ASSERT_EQ(ndt::BinOpsImpl::byteAt<2>(value), uint8_t{0b10011001});
}

TEST_F(N4BinOpsTest, GetUInt32From4Bytes)
{
    constexpr std::size_t kNBytes = 4;
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    const auto value = ndt::BinOpsImpl::get<uint32_t>(rawData_.data(), kNBytes);
    static_assert(
        std::is_same_v<std::remove_const_t<decltype(value)>, uint32_t>,
        "value is of invalid type.");
    ASSERT_EQ(value, uint32_t{0b10101111'10000001'10011001'00111100});
}

TEST_F(N8BinOpsTest, Add55BitsAt5Offset)
{
    std::array<uint8_t, 8> source{0b10101111, 0b00010111, 0b00001111,
                                  0b00110011, 0b10101010, 0b01111110,
                                  0b10000001, 0b10011001};
    constexpr ndt::DstBitOffset kDstOffset{5};
    constexpr ndt::SrcBitOffset kSrcOffset{7};
    constexpr ndt::NumBits kNBits{55};
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    rawData_[4] = 0b11000011;
    rawData_[5] = 0b11100111;
    rawData_[6] = 0b10111101;
    rawData_[7] = 0b00111100;
    ndt::BinOps::add(rawData_.data(), kDstOffset, source.data(), kSrcOffset,
                     kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101100});
    ASSERT_EQ(rawData_[1], uint8_t{0b01011100});
    ASSERT_EQ(rawData_[2], uint8_t{0b00111100});
    ASSERT_EQ(rawData_[3], uint8_t{0b11001110});
    ASSERT_EQ(rawData_[4], uint8_t{0b10101001});
    ASSERT_EQ(rawData_[5], uint8_t{0b11111010});
    ASSERT_EQ(rawData_[6], uint8_t{0b00000110});
    ASSERT_EQ(rawData_[7], uint8_t{0b01101100});
}

TEST_F(N32BinOpsTest, Add64BitsAt5Offset)
{
    constexpr uint64_t kValue =
        0b10101111'00010111'00001111'00110011'10101010'01111110'10000001'10011001;
    constexpr uint_fast8_t kOffset = 5;
    constexpr std::size_t kNBits = 64;
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    rawData_[4] = 0b11000011;
    rawData_[5] = 0b11100111;
    rawData_[6] = 0b10111101;
    rawData_[7] = 0b00111100;
    rawData_[8] = 0b11111101;
    ndt::BinOps::add(rawData_.data(), kOffset, std::move(kValue), kNBits);
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

TEST_F(N32BinOpsTest, Add60BitsAt7Offset)
{
    constexpr uint64_t kValue =
        0b10101111'00010111'00001111'00110011'10101010'01111110'10000001'10011001;
    constexpr uint_fast8_t kOffset = 7;
    constexpr std::size_t kNBits = 60;
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    rawData_[4] = 0b11000011;
    rawData_[5] = 0b11100111;
    rawData_[6] = 0b10111101;
    rawData_[7] = 0b00111100;
    rawData_[8] = 0b11111101;
    ndt::BinOps::add(rawData_.data(), kOffset, std::move(kValue), kNBits);
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

TEST_F(N4BinOpsTest, Add10BitsAt5ffset)
{
    constexpr uint32_t kValue = 0b00110011'10101010'01111110'10000001;
    constexpr uint_fast8_t kOffset = 5;
    constexpr std::size_t kNBits = 10;
    rawData_[0] = 0b10101111;
    rawData_[1] = 0b10000001;
    rawData_[2] = 0b10011001;
    rawData_[3] = 0b00111100;
    ndt::BinOps::add(rawData_.data(), kOffset, std::move(kValue), kNBits);
    ASSERT_EQ(rawData_[0], uint8_t{0b10101101});
    ASSERT_EQ(rawData_[1], uint8_t{0b00000011});
    ASSERT_EQ(rawData_[2], uint8_t{0b10011001});
    ASSERT_EQ(rawData_[3], uint8_t{0b00111100});
}

TEST_F(N1TwoBufsTest, AddHighBits0)
{
    src_[0] = 0b01101110;
    dst_[0] = 0b11101110;
    dst_[0] = ndt::BinOpsImpl::addHighBits(*dst_.data(), *src_.data(), 0);
    ASSERT_EQ(dst_[0], uint8_t{0b11101110});
}

TEST_F(N1TwoBufsTest, AddHighBits1)
{
    src_[0] = 0b01101010;
    dst_[0] = 0b11101110;
    dst_[0] = ndt::BinOpsImpl::addHighBits(*dst_.data(), *src_.data(), 1);
    ASSERT_EQ(dst_[0], uint8_t{0b01101110});
}

TEST_F(N1TwoBufsTest, AddHighBits2)
{
    src_[0] = 0b01101010;
    dst_[0] = 0b00101110;
    dst_[0] = ndt::BinOpsImpl::addHighBits(*dst_.data(), *src_.data(), 2);
    ASSERT_EQ(dst_[0], uint8_t{0b01101110});
}

TEST_F(N1TwoBufsTest, AddHighBits7)
{
    src_[0] = 0b01101011;
    dst_[0] = 0b00101110;
    dst_[0] = ndt::BinOpsImpl::addHighBits(*dst_.data(), *src_.data(), 7);
    ASSERT_EQ(dst_[0], uint8_t{0b01101010});
}

TEST_F(N1TwoBufsTest, AddHighBits8)
{
    src_[0] = 0b01101011;
    dst_[0] = 0b00101110;
    dst_[0] = ndt::BinOpsImpl::addHighBits(*dst_.data(), *src_.data(), 8);
    ASSERT_EQ(dst_[0], uint8_t{0b01101011});
}

TEST_F(N1TwoBufsTest, AddLowBits0)
{
    src_[0] = 0b01101111;
    dst_[0] = 0b00101110;
    dst_[0] = ndt::BinOpsImpl::addLowBits(*dst_.data(), *src_.data(), 0);
    ASSERT_EQ(dst_[0], uint8_t{0b00101110});
}

TEST_F(N1TwoBufsTest, AddLowBits1)
{
    src_[0] = 0b11101111;
    dst_[0] = 0b00101110;
    dst_[0] = ndt::BinOpsImpl::addLowBits(*dst_.data(), *src_.data(), 1);
    ASSERT_EQ(dst_[0], uint8_t{0b00101111});
}

TEST_F(N1TwoBufsTest, AddLowBits2)
{
    src_[0] = 0b01101110;
    dst_[0] = 0b10101001;
    dst_[0] = ndt::BinOpsImpl::addLowBits(*dst_.data(), *src_.data(), 2);
    ASSERT_EQ(dst_[0], uint8_t{0b10101010});
}

TEST_F(N1TwoBufsTest, AddLowBits7)
{
    src_[0] = 0b01101110;
    dst_[0] = 0b10101001;
    dst_[0] = ndt::BinOpsImpl::addLowBits(*dst_.data(), *src_.data(), 7);
    ASSERT_EQ(dst_[0], uint8_t{0b11101110});
}

TEST_F(N1TwoBufsTest, AddLowBits8)
{
    src_[0] = 0b01101110;
    dst_[0] = 0b10101001;
    dst_[0] = ndt::BinOpsImpl::addLowBits(*dst_.data(), *src_.data(), 8);
    ASSERT_EQ(dst_[0], uint8_t{0b01101110});
}