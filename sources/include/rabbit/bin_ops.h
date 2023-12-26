#ifndef rabbit_bin_ops_h
#define rabbit_bin_ops_h

#include <endian/endian.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include <cstdint>

#include "details.h"
#include "typedefs.h"

namespace rabbit
{
inline namespace v1
{
class Core final
{
   public:
    static constexpr void copy(Dst aDst, Src aSrc, NumBits aNBits) noexcept
    {
        using ::rabbit::details::addHighBits;
        using ::rabbit::details::copy;
        if (!aNBits)
        {
            return;
        }
        assert(aDst != nullptr && "Invalid aDst");
        assert(aSrc != nullptr && "Invalid aSrc");

        const auto bytes =
            aNBits.get() / static_cast<NumBits::value_type>(CHAR_BIT);
        const auto bits =
            aNBits.get() % static_cast<NumBits::value_type>(CHAR_BIT);
        copy(aDst, aSrc, n_bytes{bytes});
        aDst += bytes;
        aSrc += bytes;
        if (bits)
        {
            *aDst = addHighBits(*aDst, *aSrc, NumBits{bits});
        }
    }

    static constexpr void copy(Dst aDst, Src aSrc, Offset aOffset,
                               NumBits aNBits) noexcept
    {
        using ::rabbit::details::addBits;
        using ::rabbit::details::addLowBits;
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
            const std::uint_fast8_t kBitsToByteBorder =
                CHAR_BIT - aOffset.get();
            if (aNBits >= kBitsToByteBorder)
            {
                *aDst = addLowBits(*aDst, *aSrc, kBitsToByteBorder);
                ++byteOffset;
                aNBits -= NumBits(kBitsToByteBorder);
            }
            else
            {
                addBits<1>(aDst, aOffset, aNBits, *aSrc);
                return;
            }
        }
        copy(Dst(aDst + byteOffset), Src(aSrc + byteOffset), aNBits);
    }

    static constexpr void copy(Dst aDst, DstOffset aDstOffset, Src aSrc,
                               SrcOffset aSrcOffset, NumBits aNBits) noexcept
    {
        using ::rabbit::details::addBits;
        using ::rabbit::details::addValue;
        using ::rabbit::details::bytes_count;
        using ::rabbit::details::eAlign;
        using ::rabbit::details::get;
        if (!aNBits)
        {
            return;
        }
        assert(aDst != nullptr && "Invalid aDst");
        assert(aSrc != nullptr && "Invalid aSrc");
        assert(aDstOffset < CHAR_BIT && "Invalid aDstOffset");
        assert(aSrcOffset < CHAR_BIT && "Invalid aSrcOffset");

        if (aDstOffset != aSrcOffset)
        {
            std::size_t bytesWritten{};
            std::size_t bytesRead{};
            if (aDstOffset)
            {
                const std::uint_fast8_t kDstBitsToByteBorder =
                    CHAR_BIT - aDstOffset.get();
                const std::uint_fast8_t kBitsToAdd =
                    std::min(kDstBitsToByteBorder,
                             static_cast<std::uint_fast8_t>(aNBits.get()));
                const std::uint_fast8_t kSrcNBits =
                    aSrcOffset.get() + kBitsToAdd;

                const std::uint_fast8_t kNBytesToRead =
                    kSrcNBits > CHAR_BIT ? 2 : 1;
                constexpr std::size_t kNBytesToWrite = 1;

                std::uint16_t value =
                    get<std::uint16_t, eAlign::kRight>(aSrc, kNBytesToRead);
                using FastUIntT = utils::FastUInt<kNBytesToWrite>;
                const std::size_t kROffset = static_cast<std::size_t>(
                    kNBytesToRead * CHAR_BIT - kSrcNBits);
                const std::size_t kLOffset = sizeof(FastUIntT) * CHAR_BIT -
                                             aDstOffset.get() - kBitsToAdd;
                const auto kValue =
                    static_cast<FastUIntT>((value >> kROffset) << kLOffset);
                addBits(aDst, Offset{aDstOffset}, NumBits{kBitsToAdd},
                        std::move(kValue));

                aNBits -= NumBits(kBitsToAdd);

                if (!aNBits)
                {
                    return;
                }

                bytesRead += kSrcNBits / CHAR_BIT;
                bytesWritten += 1;

                aSrcOffset = SrcOffset(kSrcNBits % CHAR_BIT);
            }

            constexpr std::uint_fast8_t kMaxBitsPerAction =
                (sizeof(std::uint64_t) - 1) * CHAR_BIT;

            const std::size_t loopCount = aNBits.get() / kMaxBitsPerAction;
            std::uint64_t value{};
            for (std::size_t i = 0; i < loopCount; ++i)
            {
                constexpr std::uint_fast8_t kNBytesToRead =
                    sizeof(std::uint64_t);
                constexpr std::uint_fast8_t kNBytesToWrite = kNBytesToRead - 1;
                value = get<std::uint64_t, eAlign::kLeft>(aSrc + bytesRead,
                                                          kNBytesToRead);
                value <<= aSrcOffset.get();

                using WriteIndices = std::make_index_sequence<kNBytesToWrite>;
                addValue(aDst + bytesWritten, value, WriteIndices{});

                bytesRead += kNBytesToWrite;
                bytesWritten += kNBytesToWrite;
            }

            const auto kRestBits = aNBits % kMaxBitsPerAction;
            if (kRestBits)
            {
                std::size_t kNBytesToRead = bytes_count(kRestBits, aSrcOffset);
                value = get<std::uint64_t, eAlign::kLeft>(aSrc + bytesRead,
                                                          kNBytesToRead);
                value <<= aSrcOffset.get();
                addBits(aDst + bytesWritten, Offset{0}, kRestBits,
                        std::move(value));
            }
        }
        else
        {
            copy(aDst, aSrc, Offset(aDstOffset.get()), aNBits);
        }
    }

    template <typename T>
    static constexpr void add_value(Dst aDst, DstOffset aOffset, T &&aValue,
                                    NumBits aNBits) noexcept
    {
        using ::rabbit::details::to_byte_array;
        if (!aNBits)
        {
            return;
        }
        using UIntT = std::decay_t<T>;
        static_assert(endian::is_uint_v<UIntT>,
                      "UIntT must be unsigned integer type and not bool.");

        assert(aDst != nullptr && "Invalid aDst");
        assert(aOffset < CHAR_BIT && "Invalid aOffset");
        assert(aNBits <= sizeof(UIntT) * CHAR_BIT && "Invalid aNBits");

        const UIntT kLeftAligned = static_cast<UIntT>(
            aValue << (sizeof(UIntT) * CHAR_BIT - aNBits.get()));
        const auto kByteArray = to_byte_array(std::move(kLeftAligned));
        copy(Dst(aDst), aOffset, Src(kByteArray.data()), SrcOffset(0), aNBits);
    }

    template <typename T>
    static constexpr void add_value(Dst aDst, T &&aValue,
                                    NumBits aNBits) noexcept
    {
        add_value(aDst, DstOffset(0), std::forward<T>(aValue), aNBits);
    }

    template <typename T>
    static constexpr void add_value(Dst aDst, T &&aValue) noexcept
    {
        using ::rabbit::details::addValue;
        using UIntT = utils::remove_cvref_t<T>;
        static_assert(endian::is_uint_v<UIntT>,
                      "UIntT must be unsigned integer type and not bool.");
        using Indices = std::make_index_sequence<sizeof(UIntT)>;
        addValue(aDst, std::forward<T>(aValue), Indices{});
    }

    template <typename T>
    static constexpr T get_value(Src aSrc, SrcOffset aSrcOffset,
                                 NumBits aNBits) noexcept
    {
        using ::rabbit::details::bytes_count;
        using ::rabbit::details::bytes_to_value;
        using ::utils::num_bits;
        static_assert(endian::is_uint_v<T>,
                      "T must be unsigned integer type and not bool.");
        T result{};
        if (!aNBits)
        {
            return result;
        }

        assert(aSrc != nullptr && "Invalid aSrc");
        assert(aSrcOffset < CHAR_BIT && "Invalid aSrcOffset");

        constexpr auto kBitsInT = num_bits<T>();
        assert(aNBits <= kBitsInT && "Invalid aNBits");

        const auto kNBytesToRead = bytes_count(aNBits, aSrcOffset);
        const auto kMin = std::min(kNBytesToRead, sizeof(T));

        result = bytes_to_value<T>(aSrc, kMin);
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
    static constexpr T get_value(Src aSrc, NumBits aNBits) noexcept
    {
        using ::rabbit::details::bytes_count;
        using ::rabbit::details::bytes_to_value;
        using ::utils::num_bits;
        static_assert(endian::is_uint_v<T>,
                      "T must be unsigned integer type and not bool.");
        T result{};
        if (!aNBits)
        {
            return result;
        }

        assert(aSrc != nullptr && "Invalid aSrc");
        assert(aNBits <= num_bits<T>() && "Invalid aNBits");
        const auto kNBytesToRead = bytes_count(aNBits);
        result = bytes_to_value<T>(aSrc, kNBytesToRead);
        result >>= num_bits<T>() - aNBits.get();
        return result;
    }

    template <typename T>
    static constexpr T get_value(Src aSrc) noexcept
    {
        using ::rabbit::details::bytes_to_value;
        static_assert(endian::is_uint_v<T>,
                      "T must be unsigned integer type and not bool.");
        assert(aSrc != nullptr && "Invalid aSrc");
        return bytes_to_value<T>(aSrc);
    }
};
}  // namespace v1

namespace v2
{
namespace details
{
template <typename T, std::size_t NBytes>
constexpr T to_ule(Src aSrc, std::size_t DstIdx) noexcept;

template <typename T, std::size_t NBytes>
constexpr T to_ule(Src aSrc) noexcept;

template <std::size_t NBytes, typename T>
constexpr void to_bytes_as_le(Dst aDst, T aValue) noexcept;

template <typename T, std::size_t... I>
constexpr T to_ule_impl(Src aSrc, std::size_t DstIdx,
                        std::index_sequence<I...>) noexcept
{
    static_assert(sizeof...(I) <= sizeof(T));
    assert(DstIdx + sizeof...(I) <= sizeof(T));
    static_assert(utils::is_uint_v<T>);
    assert(aSrc);
    if constexpr (sizeof...(I))
    {
        return (... | static_cast<T>((std::to_integer<T>(aSrc[I])
                                      << (DstIdx + I) * CHAR_BIT)));
    }
    else
    {
        return T{};
    }
}

template <typename T, std::size_t... I>
constexpr T to_ule_impl(Src aSrc, std::index_sequence<I...>) noexcept
{
    static_assert(sizeof...(I) <= sizeof(T));
    static_assert(utils::is_uint_v<T>);
    assert(aSrc);
    if constexpr (sizeof...(I))
    {
        return (... |
                static_cast<T>((std::to_integer<T>(aSrc[I]) << I * CHAR_BIT)));
    }
    else
    {
        return T{};
    }
}

template <typename T, std::size_t... I>
constexpr void to_bytes_as_le_impl(Dst aDst, T aValue,
                                   std::index_sequence<I...>) noexcept
{
    static_assert(utils::is_uint_v<T>);
    static_assert(sizeof...(I) <= sizeof(T));
    assert(aDst != nullptr);
    (..., (aDst[I] = static_cast<std::byte>(aValue >> CHAR_BIT * I)));
}

template <typename T, std::size_t... I>
constexpr decltype(auto) make_to_ule_f_table_impl(
    std::index_sequence<I...>) noexcept
{
    static_assert(utils::is_uint_v<T>);
    static_assert(sizeof...(I) == sizeof(T) + 1);
    using FuncT = T (*)(Src, std::size_t) noexcept;
    return std::array<FuncT, sizeof...(I)>{to_ule<T, I>...};
}

template <typename T>
constexpr decltype(auto) make_to_ule_f_table() noexcept
{
    using Indices = std::make_index_sequence<sizeof(T) + 1>;
    return make_to_ule_f_table_impl<T>(Indices{});
}

template <typename T, std::size_t... I>
constexpr decltype(auto) make_to_bytes_as_le_f_table_impl(
    std::index_sequence<I...>) noexcept
{
    static_assert(utils::is_uint_v<T>);
    static_assert(sizeof...(I) <= sizeof(T));
    using FuncT = void (*)(Dst, T) noexcept;
    return std::array<FuncT, sizeof...(I)>{to_bytes_as_le<I + 1, T>...};
}

template <typename T>
constexpr decltype(auto) make_to_bytes_as_le_f_table() noexcept
{
    using Indices = std::make_index_sequence<sizeof(T)>;
    return make_to_bytes_as_le_f_table_impl<T>(Indices{});
}

template <typename T, std::size_t NBytes>
constexpr T to_ule(Src aSrc, std::size_t DstIdx) noexcept
{
    using Indices = std::make_index_sequence<NBytes>;
    return details::to_ule_impl<T>(aSrc, DstIdx, Indices{});
}

template <typename T, std::size_t NBytes>
constexpr T to_ule(Src aSrc) noexcept
{
    using Indices = std::make_index_sequence<NBytes>;
    return details::to_ule_impl<T>(aSrc, Indices{});
}

template <std::size_t NBytes, typename T>
constexpr inline void to_bytes_as_le(Dst aDst, T aValue) noexcept
{
    using Indices = std::make_index_sequence<NBytes>;
    details::to_bytes_as_le_impl(aDst, std::forward<T>(aValue), Indices{});
}

template <std::size_t NBytes, typename T>
constexpr inline std::size_t ule_to_bytes(Dst aDst, T aValue) noexcept
{
    using Indices = std::make_index_sequence<NBytes>;
    details::to_bytes_as_le_impl(aDst, std::forward<T>(aValue), Indices{});
    return NBytes;
}

template <typename T>
constexpr T dst_value_as_le(Src aSrc, const std::size_t aNBytes) noexcept
{
    static_assert(utils::is_uint_v<T>);
    assert(aNBytes > 0);
    assert(aNBytes <= sizeof(T));
    T value = std::to_integer<T>(aSrc[0]) |
              static_cast<T>(std::to_integer<T>(aSrc[aNBytes - 1])
                             << (aNBytes - 1) * CHAR_BIT);
    return value;
}

template <typename T>
inline constexpr auto to_ule_table = details::make_to_ule_f_table<T>();

template <typename T>
inline constexpr auto to_bytes_as_le_table =
    details::make_to_bytes_as_le_f_table<T>();

template <typename T>
constexpr inline T get_value_impl(Src aSrc, SrcOffset aSrcOffset,
                                  NumBits aNumBits) noexcept
{
    using ::utils::num_bits;
    const auto kShift = num_bits<T>() - aNumBits - aSrcOffset;
    const auto bytes = ::rabbit::details::bytes_count(aNumBits, aSrcOffset);
    auto value = to_ule_table<T>[bytes](aSrc, 0);
    value <<= kShift;
    value >>= kShift + aSrcOffset;
    return value;
}
}  // namespace details

class Core final
{
   public:
    static constexpr void copy(Dst aDst, Src aSrc, NumBits aNumBits) noexcept
    {
        using ::rabbit::bit_ops::compose;
        using ::rabbit::bit_ops::mask_right;
        using ::rabbit::details::copy;

        if (!aNumBits)
        {
            return;
        }

        assert(aDst);
        assert(aSrc);

        auto [bytes, bits] =
            utils::div(aNumBits.get(), static_cast<unsigned int>(CHAR_BIT));
        copy(aDst, aSrc, n_bytes{bytes});

        if (bits)
        {
            const auto kMask = mask_right<std::byte>(NumBits{bits});
            aDst[bytes] = compose(aDst[bytes], aSrc[bytes], kMask);
        }
    }

    static constexpr void copy(Dst aDst, Src aSrc, Offset aOffset,
                               NumBits aNumBits) noexcept
    {
        using ::rabbit::bit_ops::compose;
        using ::rabbit::bit_ops::mask;
        using ::rabbit::bit_ops::mask_right;
        using ::rabbit::details::copy;
        if (!aNumBits)
        {
            return;
        }
        assert(aDst != nullptr && "Invalid aDst");
        assert(aSrc != nullptr && "Invalid aSrc");
        assert(aOffset < CHAR_BIT && "Invalid aOffset");

        if (aOffset)
        {
            const auto to_border = static_cast<std::size_t>(CHAR_BIT - aOffset);
            const auto bits = NumBits{std::min(to_border, aNumBits.get())};
            const auto m = mask<std::byte>(bits, aOffset);
            *aDst = compose(*aDst, *aSrc, m);
            if (aNumBits == bits)
            {
                return;
            }
            aNumBits -= bits;
            ++aDst;
            ++aSrc;
        }

        Core::copy(aDst, aSrc, aNumBits);
    }

    static constexpr void copy(const Dst aDst, const DstOffset aDstOffset,
                               const Src aSrc, const SrcOffset aSrcOffset,
                               const NumBits aNumBits) noexcept
    {
        using ::rabbit::bit_ops::compose;
        using ::rabbit::bit_ops::mask;
        using ::rabbit::bit_ops::mask_left;
        using ::rabbit::bit_ops::mask_right;
        using ::rabbit::bit_ops::shift_left;
        using ::rabbit::bit_ops::shift_right;
        using ::rabbit::details::bytes_count;
        using ::rabbit::v2::details::dst_value_as_le;
        using ::rabbit::v2::details::to_bytes_as_le;
        using ::rabbit::v2::details::to_bytes_as_le_table;
        using ::rabbit::v2::details::to_ule;
        using ::rabbit::v2::details::to_ule_table;
        using ::rabbit::v2::details::ule_to_bytes;

        if (aNumBits == 0)
        {
            return;
        }
        assert(aDst != nullptr && "Invalid aDst");
        assert(aSrc != nullptr && "Invalid aSrc");
        assert(aDstOffset < CHAR_BIT && "Invalid aDstOffset");
        assert(aSrcOffset < CHAR_BIT && "Invalid aSrcOffset");

        if (aSrcOffset == aDstOffset)
        {
            copy(aDst, aSrc, Offset{aSrcOffset}, aNumBits);
        }
        else
        {
            using uint = std::uint64_t;
            const std::size_t kBytesPerLoop = sizeof(uint) - 1;

            auto [shift, ule_to_buf, kDiff, kInvDiff, kMaxOffset, kMinOffset,
                  kAdjustment] = [aDstOffset, aSrcOffset]()
            {
                if (aDstOffset > aSrcOffset)
                {
                    auto vDiff = NumBits{static_cast<NumBits::value_type>(
                        aDstOffset - aSrcOffset)};
                    auto vInvDiff = NumBits{CHAR_BIT - vDiff};
                    return std::make_tuple(shift_left<uint>,
                                           ule_to_bytes<sizeof(uint), uint>,
                                           vDiff, vInvDiff, Offset{aDstOffset},
                                           Offset{aSrcOffset}, 0_uz);
                }
                else
                {
                    auto vDiff = NumBits{static_cast<NumBits::value_type>(
                        aSrcOffset - aDstOffset)};
                    auto vInvDiff = vDiff;
                    return std::make_tuple(shift_right<uint>,
                                           ule_to_bytes<sizeof(uint) - 1, uint>,
                                           vDiff, vInvDiff, Offset{aSrcOffset},
                                           Offset{aDstOffset}, 1_uz);
                }
            }();

            auto to_read = bytes_count(aNumBits, aSrcOffset);
            auto to_write = bytes_count(aNumBits, aDstOffset);

            uint src{}, src_msb{}, dst{}, m{}, result{};
            std::size_t processed{};

            const NumBits kMaxBits = aNumBits + NumBits{kMaxOffset};
            if (kMaxBits <= sizeof(uint) * CHAR_BIT)
            {
                src = shift(to_ule_table<uint>[to_read](aSrc, 0), kDiff);
                dst = dst_value_as_le<uint>(Src{aDst}, to_write);
                m = mask<uint>(aNumBits, Offset{aDstOffset});
                result = compose(dst, src, m);
                to_bytes_as_le_table<uint>[to_write - 1](aDst, result);
                return;
            }
            else
            {
                src = to_ule<uint, sizeof(uint) - 1>(aSrc);
                src_msb = std::to_integer<uint>(aSrc[sizeof(uint) - 1]);
                src = shift(src | (src_msb << (sizeof(uint) - 1) * CHAR_BIT),
                            kDiff);
                dst = std::to_integer<uint>(*aDst);
                m = mask_left<uint>(Offset{aDstOffset});
                result = compose(dst, src, m);
                processed = ule_to_buf(aDst, result);

                while (processed <= to_write - sizeof(uint))
                {
                    src = to_ule<uint, kBytesPerLoop - 1>(
                              aSrc + processed + kAdjustment, 1) |
                          src_msb;
                    src_msb = std::to_integer<uint>(
                        aSrc[processed + kBytesPerLoop - 1 + kAdjustment]);
                    src |= (src_msb << kBytesPerLoop * CHAR_BIT);
                    src = shift_right(src, kInvDiff);
                    to_bytes_as_le<kBytesPerLoop>(aDst + processed, src);
                    processed += kBytesPerLoop;
                }
                to_write -= processed;
                to_read -= processed;
                const auto kBitsLeft = aNumBits + NumBits{aDstOffset} -
                                       NumBits{processed * CHAR_BIT};

                src = to_ule_table<uint>[to_read - kAdjustment](
                          aSrc + processed + kAdjustment, 1) |
                      src_msb;
                src = shift_right(src, kInvDiff);
                dst = static_cast<uint>(
                    std::to_integer<uint>(aDst[processed + to_write - 1])
                    << (to_write - 1) * CHAR_BIT);
                m = mask_right<uint>(kBitsLeft);
                result = compose(dst, src, m);
                to_bytes_as_le_table<uint>[to_write - 1](aDst + processed,
                                                         result);
            }
        }
    }

    template <typename T>
    static constexpr void add_value(Dst aDst, T aValue) noexcept
    {
        using ::rabbit::v2::details::ule_to_bytes;
        static_assert(endian::is_uint_v<T>,
                      "T must be unsigned integer type and not bool.");

        assert(aDst != nullptr && "Invalid aDst");

        ule_to_bytes<sizeof(T)>(aDst, aValue);
    }

    template <typename T>
    static constexpr void add_value(Dst aDst, T aValue,
                                    NumBits aNumBits) noexcept
    {
        using ::rabbit::bit_ops::compose;
        using ::rabbit::bit_ops::mask_right;
        using ::rabbit::v2::details::to_bytes_as_le_table;
        using ::utils::div;
        using ::utils::num_bits;
        static_assert(endian::is_uint_v<T>,
                      "T must be unsigned integer type and not bool.");

        assert(aDst != nullptr && "Invalid aDst");
        assert(aNumBits > 0);
        assert(aNumBits <= num_bits<T>());
        if (aNumBits == num_bits<T>())
        {
            add_value(aDst, aValue);
            return;
        }

        constexpr auto kCharBit = static_cast<NumBits::value_type>(CHAR_BIT);
        auto [bytes, bits] = div(aNumBits.get(), kCharBit);

        if (bytes)
        {
            to_bytes_as_le_table<T>[bytes - 1](aDst, aValue);
            const auto processed = bytes * kCharBit;
            aNumBits -= NumBits{processed};
            aValue >>= processed;
        }

        if (bits)
        {
            const auto m = mask_right<std::byte>(NumBits{bits});
            const auto value = static_cast<std::byte>(aValue);
            aDst[bytes] = compose(aDst[bytes], value, m);
        }
    }

    template <typename T>
    static constexpr void add_value(Dst aDst, DstOffset aDstOffset, T aValue,
                                    NumBits aNumBits) noexcept
    {
        using ::rabbit::bit_ops::compose;
        using ::rabbit::bit_ops::mask;
        using ::rabbit::bit_ops::mask_left;
        using ::rabbit::bit_ops::mask_right;
        using ::rabbit::bit_ops::shift_left;
        using ::rabbit::bit_ops::shift_right;
        using ::rabbit::details::bytes_count;
        using ::rabbit::v2::details::dst_value_as_le;
        using ::rabbit::v2::details::to_bytes_as_le;
        using ::rabbit::v2::details::to_bytes_as_le_table;
        using ::rabbit::v2::details::ule_to_bytes;
        using ::utils::num_bits;
        static_assert(endian::is_uint_v<T>,
                      "T must be unsigned integer type and not bool.");
        static_assert(sizeof(T) <= sizeof(std::uint64_t));

        assert(aDst != nullptr && "Invalid aDst");
        assert(aDstOffset < CHAR_BIT);
        assert(aNumBits > 0);
        assert(aNumBits <= num_bits<T>());

        using uint = T;
        const auto kShift = NumBits{num_bits<uint>() - aDstOffset};

        if (aNumBits <= kShift)
        {
            using fast_uint = utils::fast_uint_from_nbytes_t<sizeof(uint)>;
            const std::size_t bytes = bytes_count(aNumBits, Offset{aDstOffset});
            const auto m = mask<fast_uint>(aNumBits, Offset{aDstOffset});
            auto value = dst_value_as_le<fast_uint>(Src{aDst}, bytes);
            fast_uint fast_value = shift_left(aValue, aDstOffset);
            value = compose(value, fast_value, m);
            to_bytes_as_le_table<fast_uint>[bytes - 1](aDst, value);
        }
        else
        {
            if constexpr (sizeof(T) < sizeof(uint64_t))
            {
                using fast_uint = utils::fast_uint_from_nbytes_t<sizeof(T) + 1>;
                const std::size_t bytes = sizeof(T) + 1;
                const auto m = mask<fast_uint>(aNumBits, Offset{aDstOffset});
                auto value = dst_value_as_le<fast_uint>(Src{aDst}, bytes);
                fast_uint fast_value = static_cast<fast_uint>(aValue);
                fast_value = shift_left(fast_value, aDstOffset);
                value = compose(value, fast_value, m);
                to_bytes_as_le<bytes>(aDst, value);
            }
            else
            {
                const auto m1 = mask_left<uint>(Offset{aDstOffset});
                auto value1 = shift_left(aValue, NumBits{aDstOffset});
                value1 = compose(static_cast<uint>(*aDst), value1, m1);
                ule_to_bytes<sizeof(uint)>(aDst, value1);

                const auto m2 = mask_right<std::byte>(aNumBits - kShift);
                auto value2 =
                    static_cast<std::byte>(shift_right(aValue, kShift));
                value2 = compose(aDst[sizeof(uint)], value2, m2);
                aDst[sizeof(uint)] = value2;
            }
        }
    }

    template <typename T>
    static constexpr T get_value(Src aSrc) noexcept
    {
        using ::rabbit::v2::details::to_ule;
        static_assert(endian::is_uint_v<T>,
                      "T must be unsigned integer type and not bool.");
        assert(aSrc != nullptr && "Invalid aSrc");
        return to_ule<T, sizeof(T)>(aSrc);
    }

    template <typename T>
    static constexpr T get_value(Src aSrc, NumBits aNumBits) noexcept
    {
        using ::rabbit::details::bytes_count;
        using ::rabbit::v2::details::to_ule_table;
        using ::utils::num_bits;
        static_assert(endian::is_uint_v<T>,
                      "T must be unsigned integer type and not bool.");
        assert(aSrc != nullptr && "Invalid aSrc");
        assert(aNumBits > 0);
        assert(aNumBits <= num_bits<T>() && "Invalid aNBits");

        using uint = utils::fast_uint_from_nbytes_t<sizeof(T)>;
        const auto kShift = NumBits{sizeof(uint) * CHAR_BIT - aNumBits};
        const auto bytes = bytes_count(aNumBits);
        uint value = to_ule_table<uint>[bytes](aSrc, 0);
        value <<= kShift;
        value >>= kShift;
        return static_cast<T>(value);
    }

    template <typename T>
    static constexpr T get_value(Src aSrc, SrcOffset aSrcOffset,
                                 NumBits aNumBits) noexcept
    {
        using ::rabbit::bit_ops::mask_right;
        using ::rabbit::v2::details::get_value_impl;
        using ::rabbit::v2::details::to_ule;
        using ::utils::num_bits;
        static_assert(endian::is_uint_v<T>,
                      "T must be unsigned integer type and not bool.");
        assert(aSrc != nullptr && "Invalid aSrc");
        assert(aNumBits > 0);
        assert(aNumBits <= num_bits<T>() && "Invalid aNBits");
        assert(aSrcOffset < CHAR_BIT);

        using uint = T;
        const auto kBits = aNumBits + aSrcOffset;

        uint value{};
        if (kBits <= num_bits<T>())
        {
            value = get_value_impl<uint>(aSrc, aSrcOffset, aNumBits);
        }
        else
        {
            if constexpr (sizeof(T) < sizeof(uint64_t))
            {
                using fast_uint = utils::fast_uint_from_nbytes_t<sizeof(T) + 1>;
                value = static_cast<uint>(
                    get_value_impl<fast_uint>(aSrc, aSrcOffset, aNumBits));
            }
            else
            {
                value = to_ule<uint, sizeof(uint)>(aSrc);
                value >>= aSrcOffset;
                const auto kExcessBits = NumBits{kBits % CHAR_BIT};
                uint msb = std::to_integer<uint>(
                    aSrc[sizeof(uint)] & mask_right<std::byte>(kExcessBits));
                msb <<= num_bits<T>() - aSrcOffset;
                value |= msb;
            }
        }
        return value;
    }
};
}  // namespace v2
}  // namespace rabbit

#endif /* rabbit_bin_ops_h */
