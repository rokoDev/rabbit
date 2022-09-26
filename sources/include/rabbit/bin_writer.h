#ifndef rabbit_bin_writer_h
#define rabbit_bin_writer_h

#include <buffer/buffer.h>

#include <boost/leaf.hpp>
#include <utility>

#include "bin_ops.h"

namespace rabbit
{
namespace leaf = boost::leaf;

template <class T>
using result = leaf::result<T>;

enum class writer_error
{
    invalid_start_pos = 1,
    not_enough_buffer_size,
    num_bits_exceed_type_size,
    null_src_bits_array,
    write_more_than_source_size
};

using buf_view = buffer::buffer_view<uint8_t>;
using buf_view_const = buffer::buffer_view_const<buf_view::value_type>;
using bit_pos = buffer::bit_pos;
using n_bytes = buffer::n_bytes;

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
result<void> validate_args(const uint8_t (&)[N], NumBits aNBits) noexcept
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
class bin_writer;

template <typename ImplT>
result<bin_writer<ImplT>> make_bin_writer(buf_view aBufView,
                                          bit_pos aStartBit) noexcept;

template <typename ImplT>
class bin_writer
{
   public:
    using value_type = uint8_t;

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
            ImplT::addValue(dst(), DstBitOffset(kOffset),
                            std::forward<T>(aValue), aNBits);
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
            ImplT::addValue(dst(), std::forward<T>(aValue));
        }
        else
        {
            ImplT::addValue(dst(), DstBitOffset(kOffset),
                            std::forward<T>(aValue), NumBits(nBits.get()));
        }

        pos_ = nextPos;
        return {};
    }

    result<void> addBits(Src aSrc, SrcBitOffset aSrcOffset,
                         NumBits aNBits) noexcept
    {
        BOOST_LEAF_CHECK(details::validate_args(aSrc));
        BOOST_LEAF_AUTO(nextPos, getNextPos(aNBits));
        aSrc.get() += aSrcOffset / CHAR_BIT;
        aSrcOffset = aSrcOffset % CHAR_BIT;
        const auto kDstOffset = pos_.bitOffset();
        if (aSrcOffset != kDstOffset)
        {
            ImplT::copyBits(dst(), DstBitOffset(kDstOffset), aSrc, aSrcOffset,
                            aNBits);
        }
        else
        {
            if (kDstOffset)
            {
                ImplT::copyBits(dst(), aSrc, BitOffset(kDstOffset), aNBits);
            }
            else
            {
                ImplT::copyBits(dst(), aSrc, aNBits);
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
            ImplT::copyBits(dst(), DstBitOffset(kDstOffset), aSrc,
                            SrcBitOffset(0), aNBits);
        }
        else
        {
            ImplT::copyBits(dst(), aSrc, aNBits);
        }
        pos_ = nextPos;
        return {};
    }

    template <std::size_t N>
    result<void> addBits(const uint8_t (&aSrc)[N], NumBits aNBits) noexcept
    {
        BOOST_LEAF_CHECK(details::validate_args(aSrc, aNBits));
        return addBits(Src(aSrc), aNBits);
    }

    template <std::size_t N>
    result<void> addBits(const uint8_t (&aSrc)[N]) noexcept
    {
        return addBits(Src(aSrc), NumBits(N * CHAR_BIT));
    }

    inline constexpr buf_view_const buffer() const noexcept
    {
        return buf_view_const(buf_);
    }

    inline constexpr bit_pos pos() const noexcept { return pos_; }

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
result<bin_writer<ImplT>> make_bin_writer(uint8_t (&aData)[N],
                                          bit_pos aStartBit) noexcept
{
    BOOST_LEAF_AUTO(buf, buffer::make_bv(aData));
    return make_bin_writer<ImplT>(buf, aStartBit);
}

template <typename ImplT = Core, std::size_t N>
result<bin_writer<ImplT>> make_bin_writer(uint8_t (&aData)[N]) noexcept
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
