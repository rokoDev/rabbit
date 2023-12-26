#ifndef rabbit_bin_writer_h
#define rabbit_bin_writer_h

#include <utility>

#include "bin_ops.h"

namespace rabbit
{
enum class writer_error
{
    invalid_start_pos = 1,
    not_enough_buffer_size,
    num_bits_exceed_type_size,
    null_src_bits_array,
    write_more_than_source_size,
    value_below_interval,
    value_above_interval,
    vector_size_is_too_big
};

namespace details
{
result<void> validate_args(buf_view aBufView, bit_pos aStartBit) noexcept
{
    if (aStartBit <= aBufView.size())
    {
        return {};
    }
    return leaf::new_error(writer_error::invalid_start_pos);
}

template <std::size_t N>
result<void> validate_args(const std::byte (&)[N], NumBits aNBits) noexcept
{
    if (aNBits <= N * CHAR_BIT)
    {
        return {};
    }
    return leaf::new_error(writer_error::write_more_than_source_size);
}

result<void> validate_args(Src aSrc) noexcept
{
    if (aSrc)
    {
        return {};
    }
    return leaf::new_error(writer_error::null_src_bits_array);
}
}  // namespace details

template <typename ImplT>
class simple_bin_writer
{
   public:
    using value_type = std::byte;

    simple_bin_writer() = delete;

    inline constexpr simple_bin_writer(simple_buf_view aBufView,
                                       bit_pos aStartBit) noexcept
        : buf_(aBufView), pos_(aStartBit)
    {
        assert(aStartBit <= aBufView.size() && "Invalid aStartBit");
    }

    inline constexpr simple_bin_writer(simple_buf_view aBufView) noexcept
        : simple_bin_writer(aBufView, bit_pos(0))
    {
    }

    template <std::size_t N>
    inline constexpr simple_bin_writer(std::byte (&aData)[N],
                                       bit_pos aStartBit) noexcept
        : simple_bin_writer(simple_buf_view(aData), aStartBit)
    {
    }

    template <std::size_t N>
    inline constexpr simple_bin_writer(std::byte (&aData)[N]) noexcept
        : simple_bin_writer(aData, bit_pos(0))
    {
    }

    inline constexpr simple_bin_writer(Dst aDst, n_bytes aSize,
                                       bit_pos aStartBit) noexcept
        : simple_bin_writer(simple_buf_view(aDst.get(), aSize), aStartBit)
    {
    }

    inline constexpr simple_bin_writer(Dst aDst, n_bytes aSize) noexcept
        : simple_bin_writer(aDst, aSize, bit_pos(0))
    {
    }

    inline constexpr value_type operator[](n_bytes aIndex) const noexcept
    {
        return buf_[aIndex];
    }

    template <typename T>
    constexpr void addValue(T &&aValue, NumBits aNBits) noexcept
    {
        constexpr NumBits kMaxBits(utils::num_bits<utils::remove_cvref_t<T>>());
        assert(aNBits <= kMaxBits && "Invalid aNBits");
        if (aNBits < kMaxBits)
        {
            const auto kOffset = pos_.bitOffset();
            ImplT::add_value(dst(), DstOffset(kOffset), std::forward<T>(aValue),
                             aNBits);
            increment_pos(aNBits);
        }
        else
        {
            addValue(std::forward<T>(aValue));
        }
    }

    template <typename T>
    constexpr void addValue(T &&aValue) noexcept
    {
        constexpr NumBits nBits(utils::num_bits<utils::remove_cvref_t<T>>());
        const auto kOffset = pos_.bitOffset();
        if (!kOffset)
        {
            ImplT::add_value(dst(), std::forward<T>(aValue));
        }
        else
        {
            ImplT::add_value(dst(), DstOffset(kOffset), std::forward<T>(aValue),
                             nBits);
        }
        increment_pos(nBits);
    }

    constexpr void addBits(Src aSrc, SrcOffset aSrcOffset,
                           NumBits aNBits) noexcept
    {
        assert(aSrc && "aSrc must be not null");
        aSrc.get() += aSrcOffset / CHAR_BIT;
        aSrcOffset = aSrcOffset % CHAR_BIT;
        const auto kDstOffset = pos_.bitOffset();
        if (aSrcOffset != kDstOffset)
        {
            ImplT::copy(dst(), DstOffset(kDstOffset), aSrc, aSrcOffset, aNBits);
        }
        else
        {
            if (kDstOffset)
            {
                ImplT::copy(dst(), aSrc, Offset(kDstOffset), aNBits);
            }
            else
            {
                ImplT::copy(dst(), aSrc, aNBits);
            }
        }
        increment_pos(aNBits);
    }

    constexpr void addBits(Src aSrc, NumBits aNBits) noexcept
    {
        assert(aSrc && "aSrc must be not null");
        const auto kDstOffset = pos_.bitOffset();
        if (kDstOffset)
        {
            ImplT::copy(dst(), DstOffset(kDstOffset), aSrc, SrcOffset(0),
                        aNBits);
        }
        else
        {
            ImplT::copy(dst(), aSrc, aNBits);
        }
        increment_pos(aNBits);
    }

    template <std::size_t N>
    constexpr void addBits(const std::byte (&aSrc)[N],
                           NumBits aNBits) const noexcept
    {
        assert(aNBits <= N * CHAR_BIT &&
               "attempt to write more bits than contains in aSrc");
        addBits(Src(aSrc), aNBits);
    }

    template <std::size_t N>
    constexpr void addBits(const std::byte (&aSrc)[N]) const noexcept
    {
        addBits(Src(aSrc), NumBits(N * CHAR_BIT));
    }

    inline constexpr simple_buf_view_const buffer() const noexcept
    {
        return simple_buf_view_const(buf_);
    }

    inline constexpr bit_pos pos() const noexcept { return pos_; }

    inline constexpr std::size_t bytes_used() const noexcept
    {
        return pos_.bytesUsed();
    }

   private:
    inline constexpr void increment_pos(NumBits aNBits) noexcept
    {
        pos_ += bit_pos(aNBits);
    }

    inline constexpr Dst dst() const noexcept
    {
        return Dst(buf_.data() + pos_.byteIndex());
    }

    simple_buf_view buf_;
    bit_pos pos_;
};

template <typename ImplT>
result<bin_writer<ImplT>> make_bin_writer(buf_view aBufView,
                                          bit_pos aStartBit) noexcept;

template <typename ImplT>
class bin_writer
{
   public:
    using value_type = std::byte;

    friend result<bin_writer<ImplT>> make_bin_writer<>(
        buf_view aBufView, bit_pos aStartBit) noexcept;
    bin_writer() = delete;

    inline result<value_type> operator[](n_bytes aIndex) const noexcept
    {
        return buf_[aIndex];
    }

    template <typename T>
    result<void> addValue(T &&aValue, NumBits aNBits) noexcept
    {
        constexpr NumBits kMaxBits(utils::num_bits<utils::remove_cvref_t<T>>());
        if (aNBits < kMaxBits)
        {
            BOOST_LEAF_AUTO(nextPos, getNextPos(aNBits));
            const auto kOffset = pos_.bitOffset();
            ImplT::add_value(dst(), DstOffset(kOffset), std::forward<T>(aValue),
                             aNBits);
            pos_ = nextPos;
            return {};
        }
        else if (aNBits == kMaxBits)
        {
            return addValue(std::forward<T>(aValue));
        }
        else
        {
            return leaf::new_error(writer_error::num_bits_exceed_type_size);
        }
    }

    template <typename T>
    result<void> addValue(T &&aValue) noexcept
    {
        constexpr NumBits nBits(utils::num_bits<utils::remove_cvref_t<T>>());
        BOOST_LEAF_AUTO(nextPos, getNextPos(nBits));
        const auto kOffset = pos_.bitOffset();
        if (!kOffset)
        {
            ImplT::add_value(dst(), std::forward<T>(aValue));
        }
        else
        {
            ImplT::add_value(dst(), DstOffset(kOffset), std::forward<T>(aValue),
                             NumBits(nBits.get()));
        }

        pos_ = nextPos;
        return {};
    }

    result<void> addBits(Src aSrc, SrcOffset aSrcOffset,
                         NumBits aNBits) noexcept
    {
        BOOST_LEAF_CHECK(details::validate_args(aSrc));
        BOOST_LEAF_AUTO(nextPos, getNextPos(aNBits));
        aSrc.get() += aSrcOffset / CHAR_BIT;
        aSrcOffset = aSrcOffset % CHAR_BIT;
        const auto kDstOffset = pos_.bitOffset();
        if (aSrcOffset != kDstOffset)
        {
            ImplT::copy(dst(), DstOffset(kDstOffset), aSrc, aSrcOffset, aNBits);
        }
        else
        {
            if (kDstOffset)
            {
                ImplT::copy(dst(), aSrc, Offset(kDstOffset), aNBits);
            }
            else
            {
                ImplT::copy(dst(), aSrc, aNBits);
            }
        }
        pos_ = nextPos;
        return {};
    }

    result<void> addBits(Src aSrc, NumBits aNBits) noexcept
    {
        BOOST_LEAF_CHECK(details::validate_args(aSrc));
        BOOST_LEAF_AUTO(nextPos, getNextPos(aNBits));
        const auto kDstOffset = pos_.bitOffset();
        if (kDstOffset)
        {
            ImplT::copy(dst(), DstOffset(kDstOffset), aSrc, SrcOffset(0),
                        aNBits);
        }
        else
        {
            ImplT::copy(dst(), aSrc, aNBits);
        }
        pos_ = nextPos;
        return {};
    }

    template <std::size_t N>
    result<void> addBits(const std::byte (&aSrc)[N], NumBits aNBits) noexcept
    {
        BOOST_LEAF_CHECK(details::validate_args(aSrc, aNBits));
        return addBits(Src(aSrc), aNBits);
    }

    template <std::size_t N>
    result<void> addBits(const std::byte (&aSrc)[N]) noexcept
    {
        return addBits(Src(aSrc), NumBits(N * CHAR_BIT));
    }

    inline constexpr buf_view_const buffer() const noexcept
    {
        return buf_view_const(buf_);
    }

    inline constexpr bit_pos pos() const noexcept { return pos_; }

    inline constexpr std::size_t bytes_used() const noexcept
    {
        return pos_.bytesUsed();
    }

   private:
    result<bit_pos> getNextPos(NumBits aNBits) noexcept
    {
        const bit_pos nextPos(pos_ + bit_pos(aNBits));
        if (nextPos <= bit_pos(buf_.size()))
        {
            return nextPos;
        }
        return leaf::new_error(writer_error::not_enough_buffer_size);
    }
    inline constexpr Dst dst() const noexcept
    {
        return Dst(buf_.data() + pos_.byteIndex());
    }
    bin_writer(buf_view aBufView, bit_pos aStartBit) noexcept
        : buf_(aBufView), pos_(aStartBit)
    {
    }

    buf_view buf_;
    bit_pos pos_;
};

template <typename ImplT = Core>
result<bin_writer<ImplT>> make_bin_writer(buf_view aBufView,
                                          bit_pos aStartBit) noexcept
{
    BOOST_LEAF_CHECK(details::validate_args(aBufView, aStartBit));
    return bin_writer<ImplT>(aBufView, aStartBit);
}

template <typename ImplT = Core>
result<bin_writer<ImplT>> make_bin_writer(buf_view aBufView) noexcept
{
    return make_bin_writer<ImplT>(aBufView, bit_pos(0));
}

template <typename ImplT = Core, std::size_t N>
result<bin_writer<ImplT>> make_bin_writer(std::byte (&aData)[N],
                                          bit_pos aStartBit) noexcept
{
    BOOST_LEAF_AUTO(buf, buffer::make_bv(aData));
    return make_bin_writer<ImplT>(buf, aStartBit);
}

template <typename ImplT = Core, std::size_t N>
result<bin_writer<ImplT>> make_bin_writer(std::byte (&aData)[N]) noexcept
{
    return make_bin_writer<ImplT>(aData, bit_pos(0));
}

template <typename ImplT = Core>
result<bin_writer<ImplT>> make_bin_writer(Dst aDst, n_bytes aSize,
                                          bit_pos aStartBit) noexcept
{
    BOOST_LEAF_AUTO(buf, buffer::make_bv(aDst.get(), aSize));
    return make_bin_writer<ImplT>(buf, aStartBit);
}

template <typename ImplT = Core>
result<bin_writer<ImplT>> make_bin_writer(Dst aDst, n_bytes aSize) noexcept
{
    return make_bin_writer<ImplT>(aDst, aSize, bit_pos(0));
}
}  // namespace rabbit

#endif /* rabbit_bin_writer_h */
