#ifndef rabbit_bin_writer_h
#define rabbit_bin_writer_h

#include <utility>

#include "bin_ops.h"
#include "bit_holder.h"

namespace rabbit
{
template <typename Core, template <typename> class Tag, typename ResultAdapter,
          typename BufView>
class simple_bin_writer
    : public details::bit_holder<BufView, Tag, ResultAdapter>
{
   public:
    using base = details::bit_holder<BufView, Tag, ResultAdapter>;

    simple_bin_writer() = delete;

    inline constexpr simple_bin_writer(BufView aBufView,
                                       bit_pos aStartBit) noexcept
        : base(aBufView, aStartBit)
    {
    }

    inline constexpr simple_bin_writer(BufView aBufView) noexcept
        : simple_bin_writer(aBufView, bit_pos(0))
    {
    }

    template <std::size_t N>
    inline constexpr simple_bin_writer(std::byte (&aData)[N],
                                       bit_pos aStartBit) noexcept
        : simple_bin_writer(BufView(aData), aStartBit)
    {
    }

    template <std::size_t N>
    inline constexpr simple_bin_writer(std::byte (&aData)[N]) noexcept
        : simple_bin_writer(aData, bit_pos(0))
    {
    }

    inline constexpr simple_bin_writer(Dst aDst, n_bytes aSize,
                                       bit_pos aStartBit) noexcept
        : simple_bin_writer(BufView(aDst.get(), aSize), aStartBit)
    {
    }

    inline constexpr simple_bin_writer(Dst aDst, n_bytes aSize) noexcept
        : simple_bin_writer(aDst, aSize, bit_pos(0))
    {
    }

    template <typename T>
    constexpr void addValue(T aValue, NumBits aNBits) noexcept
    {
        constexpr NumBits kMaxBits(utils::num_bits<T>());
        assert(aNBits < kMaxBits && "Invalid aNBits");
        Core::add_value(Dst{base::operator[](base::byte_index())},
                        DstOffset{base::bit_offset()}, std::move(aValue),
                        aNBits);
        base::inc_pos(aNBits);
    }

    template <typename T>
    constexpr void addValue(T aValue) noexcept
    {
        constexpr NumBits nBits(utils::num_bits<T>());
        const auto kOffset = DstOffset{base::bit_offset()};
        if (!kOffset)
        {
            Core::add_value(Dst{base::operator[](base::byte_index())},
                            std::move(aValue));
        }
        else
        {
            Core::add_value(Dst{base::operator[](base::byte_index())}, kOffset,
                            std::move(aValue), nBits);
        }
        base::inc_pos(nBits);
    }

    constexpr void addBits(Src aSrc, SrcOffset aSrcOffset,
                           NumBits aNBits) noexcept
    {
        assert(aSrc && "aSrc must be not null");
        auto [bytes, bits] = utils::div(aSrcOffset.get(), CHAR_BIT);
        aSrc.get() += bytes;
        aSrcOffset = SrcOffset{static_cast<SrcOffset::value_type>(bits)};
        const auto kDstOffset = DstOffset{base::bit_offset()};
        if (aSrcOffset != kDstOffset)
        {
            Core::copy(Dst{base::operator[](base::byte_index())}, kDstOffset,
                       aSrc, aSrcOffset, aNBits);
        }
        else
        {
            if (kDstOffset)
            {
                Core::copy(Dst{base::operator[](base::byte_index())}, aSrc,
                           Offset(kDstOffset), aNBits);
            }
            else
            {
                Core::copy(Dst{base::operator[](base::byte_index())}, aSrc,
                           aNBits);
            }
        }
        base::inc_pos(aNBits);
    }

    constexpr void addBits(Src aSrc, NumBits aNBits) noexcept
    {
        assert(aSrc && "aSrc must be not null");
        const auto kDstOffset = DstOffset{base::bit_offset()};
        if (kDstOffset)
        {
            Core::copy(Dst{base::operator[](base::byte_index())}, kDstOffset,
                       aSrc, SrcOffset(0), aNBits);
        }
        else
        {
            Core::copy(Dst{base::operator[](base::byte_index())}, aSrc, aNBits);
        }
        base::inc_pos(aNBits);
    }

    template <std::size_t N>
    constexpr void addBits(const std::byte (&aSrc)[N], SrcOffset aOffset,
                           NumBits aNBits) noexcept
    {
        assert(aNBits <= N * CHAR_BIT &&
               "attempt to write more bits than contains in aSrc");
        addBits(Src(aSrc), aOffset, aNBits);
    }

    template <std::size_t N>
    constexpr void addBits(const std::byte (&aSrc)[N], NumBits aNBits) noexcept
    {
        assert(aNBits <= N * CHAR_BIT &&
               "attempt to write more bits than contains in aSrc");
        addBits(Src(aSrc), aNBits);
    }

    template <std::size_t N>
    constexpr void addBits(const std::byte (&aSrc)[N]) noexcept
    {
        addBits(Src(aSrc), NumBits(N * CHAR_BIT));
    }

    inline constexpr simple_buf_view_const buffer() const noexcept
    {
        return base::buf_;
    }
};
}  // namespace rabbit

#endif /* rabbit_bin_writer_h */
