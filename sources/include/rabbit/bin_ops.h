#ifndef rabbit_bin_ops_h
#define rabbit_bin_ops_h

#include <strong_type/strong_type.h>

#include <cstdint>

#include "details.h"
#include "endian.h"
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
    static constexpr void addBits(uint8_t *const aDst,
                                  uint8_t const *const aSrc,
                                  NumBits aNBits) noexcept
    {
        if (!aNBits)
        {
            return;
        }
        assert(aDst != nullptr && "Invalid aDst");
        assert(aSrc != nullptr && "Invalid aSrc");

        const std::size_t kWhole = aNBits.get() / CHAR_BIT;
        if (kWhole)
        {
            rabbit::details::copy(aDst, aSrc, kWhole);
        }

        const uint_fast8_t kFraction = aNBits.get() % CHAR_BIT;
        if (kFraction)
        {
            *(aDst + kWhole) = details::addHighBits(
                *(aDst + kWhole), *(aSrc + kWhole), kFraction);
        }
    }

    static constexpr void addBits(uint8_t *const aDst,
                                  uint8_t const *const aSrc, BitOffset aOffset,
                                  NumBits aNBits) noexcept
    {
        if (!aNBits)
        {
            return;
        }
        assert(aDst != nullptr && "Invalid aDst");
        assert(aSrc != nullptr && "Invalid aSrc");
        assert(aOffset < CHAR_BIT && "Invalid aOffset");
        std::size_t byteOffset = 0;
        if (aOffset)
        {
            const uint_fast8_t kBitsToByteBorder = CHAR_BIT - aOffset.get();
            if (aNBits >= kBitsToByteBorder)
            {
                *aDst = details::addLowBits(*aDst, *aSrc, kBitsToByteBorder);
                ++byteOffset;
                aNBits -= NumBits(kBitsToByteBorder);
            }
            else
            {
                details::addBits<1>(aDst, aOffset.get(), aNBits.get(), *aSrc);
                return;
            }
        }
        addBits(aDst + byteOffset, aSrc + byteOffset, aNBits);
    }

    static constexpr void addBits(uint8_t *const aDst, DstBitOffset aDstOffset,
                                  uint8_t const *const aSrc,
                                  SrcBitOffset aSrcOffset,
                                  NumBits aNBits) noexcept
    {
        if (!aNBits)
        {
            return;
        }
        assert(aDst != nullptr && "Invalid aDst");
        assert(aSrc != nullptr && "Invalid aSrc");
        assert(aDstOffset < CHAR_BIT && "Invalid aDstOffset");
        assert(aSrcOffset < CHAR_BIT && "Invalid aSrcOffset");

        using eAlign = details::eAlign;

        if (aDstOffset != aSrcOffset)
        {
            std::size_t bytesWritten{};
            std::size_t bytesRead{};
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

                uint16_t value =
                    details::get<uint16_t, eAlign::kRight>(aSrc, kNBytesToRead);
                using FastUIntT = FastUInt<kNBytesToWrite>;
                const std::size_t kROffset =
                    kNBytesToRead * CHAR_BIT - kSrcNBits;
                const std::size_t kLOffset = sizeof(FastUIntT) * CHAR_BIT -
                                             aDstOffset.get() - kBitsToAdd;
                const auto kValue =
                    static_cast<FastUIntT>((value >> kROffset) << kLOffset);
                details::addBits(aDst, aDstOffset.get(), kBitsToAdd,
                                 std::move(kValue));

                aNBits -= kBitsToAdd;

                if (!aNBits)
                {
                    return;
                }

                bytesRead += kSrcNBits / CHAR_BIT;
                bytesWritten += 1;

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
                value = details::get<uint64_t, eAlign::kLeft>(aSrc + bytesRead,
                                                              kNBytesToRead);
                value <<= aSrcOffset.get();

                using WriteIndices = std::make_index_sequence<kNBytesToWrite>;
                details::addValue(aDst + bytesWritten, value, WriteIndices{});

                bytesRead += kNBytesToWrite;
                bytesWritten += kNBytesToWrite;
            }

            const auto kRestBits = aNBits % kMaxBitsPerAction;
            if (kRestBits)
            {
                std::size_t kNBytesToRead =
                    details::bytesCount(aSrcOffset.get(), kRestBits.get());
                value = details::get<uint64_t, eAlign::kLeft>(aSrc + bytesRead,
                                                              kNBytesToRead);
                value <<= aSrcOffset.get();
                details::addBits(aDst + bytesWritten, 0, kRestBits.get(),
                                 std::move(value));
            }
        }
        else
        {
            addBits(aDst, aSrc, BitOffset(aDstOffset.get()), aNBits);
        }
    }

    template <typename T>
    static constexpr void addValue(uint8_t *const aDst, DstBitOffset aOffset,
                                   T &&aValue, NumBits aNBits) noexcept
    {
        if (!aNBits)
        {
            return;
        }
        using UIntT = std::decay_t<T>;
        static_assert(is_uint_v<UIntT>,
                      "UIntT must be unsigned integer type and not bool.");

        assert(aDst != nullptr && "Invalid aDst");
        assert(aOffset < CHAR_BIT && "Invalid aOffset");
        assert(aNBits <= sizeof(UIntT) * CHAR_BIT && "Invalid aNBits");

        const UIntT kLeftAligned = static_cast<UIntT>(
            aValue << (sizeof(UIntT) * CHAR_BIT - aNBits.get()));
        const auto kByteArray =
            details::to_uint8_array(std::move(kLeftAligned));
        addBits(aDst, aOffset, kByteArray.data(), SrcBitOffset(0), aNBits);
    }

    template <typename T>
    static constexpr void addValue(uint8_t *const aDst, T &&aValue,
                                   NumBits aNBits) noexcept
    {
        addValue(aDst, DstBitOffset(0), std::forward<T>(aValue), aNBits);
    }

    template <typename T>
    static constexpr void addValue(uint8_t *const aDst, T &&aValue) noexcept
    {
        using UIntT = std::remove_cv_t<std::remove_reference_t<T>>;
        static_assert(is_uint_v<UIntT>,
                      "UIntT must be unsigned integer type and not bool.");
        using Indices = std::make_index_sequence<sizeof(UIntT)>;
        details::addValue(aDst, std::forward<T>(aValue), Indices{});
    }

    template <typename T>
    static constexpr T getValue(uint8_t const *const aSrc,
                                SrcBitOffset aSrcOffset,
                                NumBits aNBits) noexcept
    {
        static_assert(is_uint_v<T>,
                      "T must be unsigned integer type and not bool.");
        T result{};
        if (!aNBits)
        {
            return result;
        }

        assert(aSrc != nullptr && "Invalid aSrc");
        assert(aSrcOffset < CHAR_BIT && "Invalid aSrcOffset");

        constexpr auto kBitsInT = utils::num_bits<T>();
        assert(aNBits <= kBitsInT && "Invalid aNBits");

        const auto kNBytesToRead =
            details::bytesCount(aSrcOffset.get(), aNBits.get());
        const auto kMin = std::min(kNBytesToRead, sizeof(T));

        result = details::uint8_buf_to_value<T>(aSrc, kMin);
        const auto rOffset = kBitsInT - aNBits.get();
        result = static_cast<T>(static_cast<T>(result << aSrcOffset.get()) >>
                                rOffset);

        if (kNBytesToRead > sizeof(T))
        {
            const auto kOneByteOffset =
                kBitsInT + CHAR_BIT - aSrcOffset.get() - aNBits.get();
            result |= static_cast<T>(aSrc[sizeof(T)] >> kOneByteOffset);
        }

        return result;
    }

    template <typename T>
    static constexpr T getValue(uint8_t const *const aSrc,
                                NumBits aNBits) noexcept
    {
        static_assert(is_uint_v<T>,
                      "T must be unsigned integer type and not bool.");
        T result{};
        if (!aNBits)
        {
            return result;
        }

        assert(aSrc != nullptr && "Invalid aSrc");
        assert(aNBits <= utils::num_bits<T>() && "Invalid aNBits");
        const auto kNBytesToRead = details::bytesCount(aNBits.get());
        result = details::uint8_buf_to_value<T>(aSrc, kNBytesToRead);
        result >>= utils::num_bits<T>() - aNBits.get();
        return result;
    }

    template <typename T>
    static constexpr T getValue(uint8_t const *const aSrc) noexcept
    {
        static_assert(is_uint_v<T>,
                      "T must be unsigned integer type and not bool.");
        assert(aSrc != nullptr && "Invalid aSrc");
        return details::uint8_buf_to_value<T>(aSrc);
    }
};
}  // namespace rabbit

#endif /* rabbit_bin_ops_h */