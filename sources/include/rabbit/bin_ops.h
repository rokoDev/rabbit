#ifndef rabbit_bin_ops_h
#define rabbit_bin_ops_h

#include <strong_type/strong_type.h>

#include <cstdint>

#include "details.h"
#include "utils.h"

namespace rabbit
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

class BinOps final
{
   public:
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
            *(aDst + kWhole) = details::Operation::addHighBits(
                *(aDst + kWhole), *(aSrc + kWhole), kFraction);
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
                *aDst = details::Operation::addLowBits(*aDst, *aSrc,
                                                       kBitsToByteBorder);
                ++byteOffset;
                aNBits -= NumBits(kBitsToByteBorder);
            }
            else
            {
                details::Operation::addBits<1>(aDst, aOffset.get(),
                                               aNBits.get(), *aSrc);
                return;
            }
        }
        addBits(aDst + byteOffset, aSrc + byteOffset, aNBits);
    }

    static constexpr void addBits(uint8_t *aDst, DstBitOffset aDstOffset,
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

        if (aDstOffset != aSrcOffset)
        {
            if (aDstOffset)
            {
                const uint_fast8_t kDstBitsToByteBorder =
                    CHAR_BIT - aDstOffset.get();
                const uint_fast8_t kBitsToAdd =
                    std::min(kDstBitsToByteBorder,
                             static_cast<uint_fast8_t>(aNBits.get()));
                const uint_fast8_t kSrcNBits = aSrcOffset.get() + kBitsToAdd;

                const uint_fast8_t kNBytesToRead = kSrcNBits > CHAR_BIT ? 2 : 1;
                constexpr std::size_t kNBytesToWrite = 1;

                uint64_t value = valueReaders_[kNBytesToRead - 1](aSrc) >>
                                 (kNBytesToRead * CHAR_BIT - kSrcNBits);
                details::Operation::addBitsFromValue<kNBytesToWrite>(
                    aDst, aDstOffset.get(), value, kBitsToAdd);

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
                details::Operation::get<uint64_t>(
                    reinterpret_cast<uint8_t *>(&value), aSrc, ReadIndices{});

                value <<= aSrcOffset.get();

                using WriteIndices = std::make_index_sequence<kNBytesToWrite>;
                details::Operation::addValue(aDst, value, WriteIndices{});

                aSrc += kNBytesToWrite;
                aDst += kNBytesToWrite;
            }

            const auto kRestBits = aNBits % kMaxBitsPerAction;
            if (kRestBits)
            {
                std::size_t kNBytesToRead = details::Operation::bytesCount(
                    aSrcOffset.get(), kRestBits.get());
                value = valueReaders_[kNBytesToRead - 1](aSrc) >>
                        (kNBytesToRead * CHAR_BIT - aSrcOffset.get() -
                         kRestBits.get());

                std::size_t kNBytesToWrite =
                    details::Operation::bytesCount(0, kRestBits.get());
                bitAdders_[kNBytesToWrite - 1](aDst, 0, value, kRestBits.get());
            }
        }
        else
        {
            addBits(aDst, aSrc, BitOffset(aDstOffset.get()), aNBits);
        }
    }

    template <typename T>
    static constexpr void addValue(uint8_t *aDst, DstBitOffset aOffset,
                                   T &&aValue, NumBits aNBits) noexcept
    {
        using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
        static_assert(is_uint_v<UIntT>,
                      "UIntT must be unsigned integer type and not bool.");

        assert(aDst != nullptr && "Invalid aDst");
        assert(aOffset < CHAR_BIT && "Invalid aOffset");
        assert(aNBits <= sizeof(UIntT) * CHAR_BIT && "Invalid aNBits");
        if (aNBits == NumBits(0))
        {
            return;
        }

        if (aNBits + aOffset <= sizeof(uint64_t) * CHAR_BIT)
        {
            const UIntT leftAligned = static_cast<UIntT>(
                aValue << (sizeof(UIntT) * CHAR_BIT - aNBits.get()));
            const UIntT netLeftAligned = toNet(leftAligned);
            addBits(aDst, aOffset,
                    reinterpret_cast<uint8_t const *>(&netLeftAligned),
                    SrcBitOffset(0), aNBits);
        }
        else
        {
            const uint_fast8_t kRest = static_cast<uint_fast8_t>(
                sizeof(uint64_t) * CHAR_BIT - aNBits.get());
            const uint64_t noLeadingZeros =
                static_cast<uint64_t>(aValue << kRest);
            const uint64_t shiftedVal = noLeadingZeros >> aOffset.get();
            details::Operation::addUInt64High(
                aDst, sizeof(uint64_t) * CHAR_BIT - aOffset.get(),
                std::move(shiftedVal));

            const uint_fast8_t kOverlap = aOffset.get() - kRest;
            details::Operation::addUInt64Low(
                aDst + sizeof(uint64_t), kOverlap,
                details::Operation::byteAt<sizeof(uint64_t) - 1>(
                    std::forward<T>(aValue)));
        }
    }

    template <typename T>
    static constexpr void addValue(uint8_t *aDst, T &&aValue,
                                   NumBits aNBits) noexcept
    {
        using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
        static_assert(is_uint_v<UIntT>,
                      "UIntT must be unsigned integer type and not bool.");

        UIntT lAlignedValue = static_cast<UIntT>(
            aValue << (sizeof(UIntT) * CHAR_BIT - aNBits.get()));
        const UIntT readyValue = toNet(std::move(lAlignedValue));
        addBits(aDst, reinterpret_cast<uint8_t const *>(&readyValue), aNBits);
    }

    template <typename T>
    static constexpr void addValue(uint8_t *aDst, T &&aValue) noexcept
    {
        using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
        static_assert(is_uint_v<UIntT>,
                      "UIntT must be unsigned integer type and not bool.");
        using Indices = std::make_index_sequence<sizeof(UIntT)>;
        details::Operation::addValue(aDst, std::forward<T>(aValue), Indices{});
    }

   private:
    static constexpr auto bitAdders_ =
        details::Operation::createBitsAdderList();
    static constexpr auto valueReaders_ =
        details::Operation::createValueReaders();
};
}  // namespace rabbit

#endif /* rabbit_bin_ops_h */