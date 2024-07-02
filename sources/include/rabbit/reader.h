#ifndef rabbit_reader_h
#define rabbit_reader_h

#include <utility>

#include "bin_ops.h"
#include "bit_holder.h"

namespace rabbit
{
template <typename Core, template <typename> class Tag, typename ResultAdapter,
          typename BufView>
class reader : public details::bit_holder<BufView, Tag, ResultAdapter>
{
   public:
    using base = details::bit_holder<BufView, Tag, ResultAdapter>;

    reader() = delete;

    inline constexpr reader(BufView aBufView, bit_pos aStartBit) noexcept
        : base(aBufView, aStartBit)
    {
    }

    inline constexpr reader(BufView aBufView) noexcept
        : reader(aBufView, bit_pos(0))
    {
    }

    template <std::size_t N>
    inline constexpr reader(const std::byte (&aData)[N],
                            bit_pos aStartBit) noexcept
        : reader(BufView(aData), aStartBit)
    {
    }

    template <std::size_t N>
    inline constexpr reader(const std::byte (&aData)[N]) noexcept
        : reader(BufView(aData), bit_pos(0))
    {
    }

    inline constexpr reader(Src aSrc, n_bytes aSize, bit_pos aStartBit) noexcept
        : reader(BufView(aSrc.get(), aSize), aStartBit)
    {
    }

    inline constexpr reader(Src aSrc, n_bytes aSize) noexcept
        : reader(aSrc, aSize, bit_pos(0))
    {
    }

    template <typename T>
    constexpr T getValue(NumBits aNBits) noexcept
    {
        auto r =
            readValue<T>(Src{base::operator[](base::byte_index())}, aNBits);
        base::inc_pos(aNBits);
        return std::move(r);
    }

    template <typename T>
    constexpr T getValue() noexcept
    {
        auto r = readValue<T>(Src{base::operator[](base::byte_index())});
        base::inc_pos(NumBits{utils::num_bits<T>()});
        return std::move(r);
    }

    constexpr void getBits(Dst aDst, DstOffset aOffset, NumBits aNBits) noexcept
    {
        readBits(aDst, aOffset, aNBits);
        base::inc_pos(aNBits);
    }

    template <std::size_t N>
    constexpr void getBits(std::byte (&aDst)[N], DstOffset aOffset,
                           NumBits aNBits) noexcept
    {
        getBits(Dst{aDst}, aOffset, aNBits);
    }

    template <std::size_t N>
    constexpr void getBits(std::byte (&aDst)[N], DstOffset aOffset) noexcept
    {
        assert(N * CHAR_BIT > aOffset);
        NumBits nBits(N * CHAR_BIT - aOffset.get());
        getBits(Dst{aDst}, aOffset, nBits);
    }

    template <std::size_t N>
    constexpr void getBits(std::byte (&aDst)[N], NumBits aNBits) noexcept
    {
        getBits(Dst{aDst}, DstOffset(0), aNBits);
    }

    template <std::size_t N>
    constexpr void getBits(std::byte (&aDst)[N]) noexcept
    {
        constexpr NumBits nBits(N * CHAR_BIT);
        getBits(Dst{aDst}, DstOffset(0), nBits);
    }

   private:
    constexpr void readBits(Dst aDst, DstOffset aOffset,
                            NumBits aNBits) noexcept
    {
        Dst kDst(aDst + aOffset / CHAR_BIT);
        DstOffset kOffset(aOffset %
                          static_cast<DstOffset::value_type>(CHAR_BIT));
        const auto kSrcOffset = base::bit_offset();
        if (kOffset != kSrcOffset)
        {
            Core::copy(kDst, kOffset, Src{base::operator[](base::byte_index())},
                       SrcOffset(kSrcOffset), aNBits);
        }
        else
        {
            if (kOffset)
            {
                Core::copy(kDst, Src{base::operator[](base::byte_index())},
                           Offset(kOffset.get()), aNBits);
            }
            else
            {
                Core::copy(kDst, Src{base::operator[](base::byte_index())},
                           aNBits);
            }
        }
    }

    template <typename T>
    constexpr T readValue(Src aSrc, NumBits aNBits) const noexcept
    {
        if (const auto kOffset = base::bit_offset(); not kOffset)
        {
            constexpr NumBits kTSize(
                utils::num_bits<utils::remove_cvref_t<T>>());
            assert((aNBits <= kTSize) &&
                   "error: number bits to read can't exceed type size.");
            if (aNBits < kTSize)
            {
                return Core::template get_value<T>(aSrc, aNBits);
            }
            else
            {
                return Core::template get_value<T>(aSrc);
            }
        }
        else
        {
            return Core::template get_value<T>(aSrc, SrcOffset(kOffset),
                                               aNBits);
        }
    }

    template <typename T>
    constexpr T readValue(Src aSrc) const noexcept
    {
        constexpr NumBits kTSize(utils::num_bits<utils::remove_cvref_t<T>>());
        if (const auto kOffset = base::bit_offset(); not kOffset)
        {
            return Core::template get_value<T>(aSrc);
        }
        else
        {
            return Core::template get_value<T>(aSrc, SrcOffset(kOffset),
                                               kTSize);
        }
    }
};
}  // namespace rabbit

#endif /* rabbit_reader_h */
