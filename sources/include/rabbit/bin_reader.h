#ifndef rabbit_bin_reader_h
#define rabbit_bin_reader_h

#include <utility>

#include "bin_ops.h"

namespace rabbit
{
enum class reader_error
{
    invalid_start_pos = 1,
    not_enough_buffer_size,
    num_bits_exceed_type_size,
    null_dst_bits_array,
    invalid_destination,
    dst_offset_too_big,
    read_more_than_destination_size,
    invalid_interval_index,
    non_empty_vector_size_is_zero
};

namespace details
{
result<void> validate_args(buf_view_const aBufView, bit_pos aStartBit) noexcept
{
    if (aStartBit <= aBufView.size())
    {
        return {};
    }
    return leaf::new_error(reader_error::invalid_start_pos);
}

template <typename T>
result<void> validate_args(NumBits aNBits) noexcept
{
    constexpr NumBits kMaxBits(utils::num_bits<utils::remove_cvref_t<T>>());
    if (aNBits <= kMaxBits)
    {
        return {};
    }
    return leaf::new_error(reader_error::num_bits_exceed_type_size);
}

result<void> validate_args(Dst aDst) noexcept
{
    if (aDst)
    {
        return {};
    }
    return leaf::new_error(reader_error::null_dst_bits_array);
}

template <std::size_t N>
result<void> validate_args(uint8_t (&)[N], DstBitOffset aOffset,
                           NumBits aNBits) noexcept
{
    if (aOffset.get() + aNBits.get() <= N * CHAR_BIT)
    {
        return {};
    }
    return leaf::new_error(reader_error::invalid_destination);
}

template <std::size_t N>
result<void> validate_args(uint8_t (&)[N], DstBitOffset aOffset) noexcept
{
    if (aOffset <= N * CHAR_BIT)
    {
        return {};
    }
    return leaf::new_error(reader_error::dst_offset_too_big);
}

template <std::size_t N>
result<void> validate_args(uint8_t (&)[N], NumBits aNBits) noexcept
{
    if (aNBits <= N * CHAR_BIT)
    {
        return {};
    }
    return leaf::new_error(reader_error::read_more_than_destination_size);
}
}  // namespace details

template <typename ImplT>
class simple_bin_reader
{
   public:
    using value_type = uint8_t;

    simple_bin_reader() = delete;

    inline constexpr simple_bin_reader(simple_buf_view_const aBufView,
                                       bit_pos aStartBit) noexcept
        : buf_(aBufView), pos_(aStartBit)
    {
        assert(aStartBit <= aBufView.size() && "Invalid aStartBit");
    }

    inline constexpr simple_bin_reader(simple_buf_view_const aBufView) noexcept
        : simple_bin_reader(aBufView, bit_pos(0))
    {
    }

    template <std::size_t N>
    inline constexpr simple_bin_reader(const uint8_t (&aData)[N],
                                       bit_pos aStartBit) noexcept
        : simple_bin_reader(simple_buf_view_const(aData), aStartBit)
    {
    }

    template <std::size_t N>
    inline constexpr simple_bin_reader(const uint8_t (&aData)[N]) noexcept
        : simple_bin_reader(simple_buf_view_const(aData), bit_pos(0))
    {
    }

    inline constexpr simple_bin_reader(Src aSrc, n_bytes aSize,
                                       bit_pos aStartBit) noexcept
        : simple_bin_reader(simple_buf_view_const(aSrc.get(), aSize), aStartBit)
    {
    }

    inline constexpr simple_bin_reader(Src aSrc, n_bytes aSize) noexcept
        : simple_bin_reader(aSrc, aSize, bit_pos(0))
    {
    }

    inline constexpr value_type operator[](n_bytes aIndex) const noexcept
    {
        return buf_[aIndex];
    }

    template <typename T>
    constexpr eReaderError getValue(T& aValue, NumBits aNBits) noexcept
    {
        auto valueGetter = [&aValue, aNBits, this]()
        { aValue = readValue<T>(src(), aNBits); };
        return getValueImpl(aNBits, std::move(valueGetter));
    }

    template <typename T>
    constexpr eReaderError getValue(T& aValue) noexcept
    {
        auto valueGetter = [&aValue, this]() { aValue = readValue<T>(src()); };
        constexpr NumBits kBitsToRead(utils::num_bits<T>());
        return getValueImpl(kBitsToRead, std::move(valueGetter));
    }

    constexpr eReaderError getBits(Dst aDst, DstBitOffset aOffset,
                                   NumBits aNBits) noexcept
    {
        auto bitsGetter = [aDst, aOffset, aNBits, this]()
        { readBits(aDst.get(), aOffset, aNBits); };
        return getValueImpl(aNBits, std::move(bitsGetter));
    }

    template <std::size_t N>
    constexpr eReaderError getBits(uint8_t (&aDst)[N], DstBitOffset aOffset,
                                   NumBits aNBits) noexcept
    {
        auto bitsGetter = [&aDst, aOffset, aNBits, this]()
        { readBits(aDst, aOffset, aNBits); };
        return getValueImpl(aNBits, std::move(bitsGetter));
    }

    template <std::size_t N>
    constexpr eReaderError getBits(uint8_t (&aDst)[N],
                                   DstBitOffset aOffset) noexcept
    {
        NumBits nBits(N * CHAR_BIT - aOffset.get());
        auto bitsGetter = [&aDst, aOffset, nBits, this]()
        { readBits(aDst, aOffset, nBits); };
        return getValueImpl(nBits, std::move(bitsGetter));
    }

    template <std::size_t N>
    constexpr eReaderError getBits(uint8_t (&aDst)[N], NumBits aNBits) noexcept
    {
        auto bitsGetter = [&aDst, aNBits, this]()
        { readBits(aDst, DstBitOffset(0), aNBits); };
        return getValueImpl(aNBits, std::move(bitsGetter));
    }

    template <std::size_t N>
    constexpr eReaderError getBits(uint8_t (&aDst)[N]) noexcept
    {
        constexpr NumBits nBits(N * CHAR_BIT);
        auto bitsGetter = [&aDst, nBits, this]()
        { readBits(aDst, DstBitOffset(0), nBits); };
        return getValueImpl(nBits, std::move(bitsGetter));
    }

    inline constexpr uint_fast8_t bitOffset() const noexcept
    {
        return pos_.bitOffset();
    }

    inline constexpr simple_buf_view_const buffer() const noexcept
    {
        return buf_;
    }

    inline constexpr bit_pos pos() const noexcept { return pos_; }

    inline constexpr std::size_t bytes_used() const noexcept
    {
        return pos_.bytesUsed();
    }

   private:
    template <typename Lambda>
    constexpr eReaderError getValueImpl(NumBits aNBits,
                                        Lambda&& aLambda) noexcept
    {
        const auto kNextPos = next_pos(aNBits);
        if (kNextPos <= bit_pos(buf_.size()))
        {
            aLambda();
            pos_ = kNextPos;
            return eReaderError::kSuccess;
        }
        else
        {
            return eReaderError::kNotEnoughBufferSize;
        }
    }

    void readBits(uint8_t* aDst, DstBitOffset aOffset, NumBits aNBits) noexcept
    {
        Dst kDst(aDst + aOffset / CHAR_BIT);
        DstBitOffset kOffset(aOffset % CHAR_BIT);
        const auto kSrcOffset = bitOffset();
        if (kOffset != kSrcOffset)
        {
            ImplT::copyBits(kDst, kOffset, src(), SrcBitOffset(kSrcOffset),
                            aNBits);
        }
        else
        {
            if (kOffset)
            {
                ImplT::copyBits(kDst, src(), BitOffset(kOffset.get()), aNBits);
            }
            else
            {
                ImplT::copyBits(kDst, src(), aNBits);
            }
        }
    }

    template <typename T>
    constexpr T readValue(Src aSrc, NumBits aNBits) const noexcept
    {
        if (const auto kOffset = bitOffset(); not kOffset)
        {
            constexpr NumBits kTSize(
                utils::num_bits<utils::remove_cvref_t<T>>());
            assert((aNBits <= kTSize) &&
                   "error: number bits to read can't exceed type size.");
            if (aNBits < kTSize)
            {
                return ImplT::template getValue<T>(aSrc, aNBits);
            }
            else
            {
                return ImplT::template getValue<T>(aSrc);
            }
        }
        else
        {
            return ImplT::template getValue<T>(aSrc, SrcBitOffset(kOffset),
                                               aNBits);
        }
    }

    template <typename T>
    constexpr T readValue(Src aSrc) const noexcept
    {
        constexpr NumBits kTSize(utils::num_bits<utils::remove_cvref_t<T>>());
        if (const auto kOffset = bitOffset(); not kOffset)
        {
            return ImplT::template getValue<T>(aSrc);
        }
        else
        {
            return ImplT::template getValue<T>(aSrc, SrcBitOffset(kOffset),
                                               kTSize);
        }
    }

    constexpr bit_pos next_pos(NumBits aNBits) const noexcept
    {
        return pos_ + bit_pos(aNBits);
    }

    inline constexpr Src src() const noexcept
    {
        return Src(buf_.data() + pos_.byteIndex());
    }

    simple_buf_view_const buf_;
    bit_pos pos_;
};

template <typename ImplT>
result<bin_reader<ImplT>> make_bin_reader(buf_view_const aBufView,
                                          bit_pos aStartBit) noexcept;

template <typename ImplT>
class bin_reader
{
   public:
    using value_type = uint8_t;

    friend result<bin_reader<ImplT>> make_bin_reader<>(
        buf_view_const aBufView, bit_pos aStartBit) noexcept;

    bin_reader() = delete;

    inline result<value_type> operator[](n_bytes aIndex) const noexcept
    {
        return buf_[aIndex];
    }

    template <typename T>
    result<T> getValue(NumBits aNBits) noexcept
    {
        BOOST_LEAF_CHECK(details::validate_args<T>(aNBits));
        BOOST_LEAF_AUTO(nextPos, getNextPos(aNBits));
        const T v = readValue<T>(src(), aNBits);
        pos_ = nextPos;
        return v;
    }

    template <typename T>
    result<T> getValue() noexcept
    {
        constexpr NumBits kBitsToRead(
            utils::num_bits<utils::remove_cvref_t<T>>());
        BOOST_LEAF_AUTO(nextPos, getNextPos(kBitsToRead));
        const T v = readValue<T>(src());
        pos_ = nextPos;
        return v;
    }

    result<void> getBits(Dst aDst, DstBitOffset aOffset,
                         NumBits aNBits) noexcept
    {
        BOOST_LEAF_CHECK(details::validate_args(aDst));
        BOOST_LEAF_AUTO(nextPos, getNextPos(aNBits));
        return readBits(aDst.get(), aOffset, aNBits, nextPos);
    }

    template <std::size_t N>
    result<void> getBits(uint8_t (&aDst)[N], DstBitOffset aOffset,
                         NumBits aNBits) noexcept
    {
        BOOST_LEAF_CHECK(details::validate_args(aDst, aOffset, aNBits));
        BOOST_LEAF_AUTO(nextPos, getNextPos(aNBits));
        return readBits(aDst, aOffset, aNBits, nextPos);
    }

    template <std::size_t N>
    result<void> getBits(uint8_t (&aDst)[N], DstBitOffset aOffset) noexcept
    {
        BOOST_LEAF_CHECK(details::validate_args(aDst, aOffset));
        NumBits nBits(N * CHAR_BIT - aOffset.get());
        BOOST_LEAF_AUTO(nextPos, getNextPos(nBits));
        return readBits(aDst, aOffset, nBits, nextPos);
    }

    template <std::size_t N>
    result<void> getBits(uint8_t (&aDst)[N], NumBits aNBits) noexcept
    {
        BOOST_LEAF_CHECK(details::validate_args(aDst, aNBits));
        BOOST_LEAF_AUTO(nextPos, getNextPos(aNBits));
        return readBits(aDst, DstBitOffset(0), aNBits, nextPos);
    }

    template <std::size_t N>
    result<void> getBits(uint8_t (&aDst)[N]) noexcept
    {
        constexpr NumBits nBits(N * CHAR_BIT);
        BOOST_LEAF_AUTO(nextPos, getNextPos(nBits));
        return readBits(aDst, DstBitOffset(0), nBits, nextPos);
    }

    inline constexpr uint_fast8_t bitOffset() const noexcept
    {
        return pos_.bitOffset();
    }

    inline constexpr buf_view_const buffer() const noexcept { return buf_; }

    inline constexpr bit_pos pos() const noexcept { return pos_; }

    inline constexpr std::size_t bytes_used() const noexcept
    {
        return pos_.bytesUsed();
    }

   private:
    constexpr bin_reader(buf_view_const aBufView, bit_pos aStartPos) noexcept
        : buf_(aBufView), pos_(aStartPos)
    {
    }

    result<void> readBits(uint8_t* aDst, DstBitOffset aOffset, NumBits aNBits,
                          bit_pos aNextPos) noexcept
    {
        Dst kDst(aDst + aOffset / CHAR_BIT);
        DstBitOffset kOffset(aOffset % CHAR_BIT);
        const auto kSrcOffset = bitOffset();
        if (kOffset != kSrcOffset)
        {
            ImplT::copyBits(kDst, kOffset, src(), SrcBitOffset(kSrcOffset),
                            aNBits);
        }
        else
        {
            if (kOffset)
            {
                ImplT::copyBits(kDst, src(), BitOffset(kOffset.get()), aNBits);
            }
            else
            {
                ImplT::copyBits(kDst, src(), aNBits);
            }
        }

        pos_ = aNextPos;
        return {};
    }

    template <typename T>
    constexpr T readValue(Src aSrc, NumBits aNBits) const noexcept
    {
        if (const auto kOffset = bitOffset(); not kOffset)
        {
            constexpr NumBits kTSize(
                utils::num_bits<utils::remove_cvref_t<T>>());
            assert((aNBits <= kTSize) &&
                   "error: number bits to read can't exceed type size.");
            if (aNBits < kTSize)
            {
                return ImplT::template getValue<T>(aSrc, aNBits);
            }
            else
            {
                return ImplT::template getValue<T>(aSrc);
            }
        }
        else
        {
            return ImplT::template getValue<T>(aSrc, SrcBitOffset(kOffset),
                                               aNBits);
        }
    }

    template <typename T>
    constexpr T readValue(Src aSrc) const noexcept
    {
        constexpr NumBits kTSize(utils::num_bits<utils::remove_cvref_t<T>>());
        if (const auto kOffset = bitOffset(); not kOffset)
        {
            return ImplT::template getValue<T>(aSrc);
        }
        else
        {
            return ImplT::template getValue<T>(aSrc, SrcBitOffset(kOffset),
                                               kTSize);
        }
    }

    result<bit_pos> getNextPos(NumBits aNBits) noexcept
    {
        const bit_pos nextPos(pos_ + bit_pos(aNBits));
        if (nextPos <= bit_pos(buf_.size()))
        {
            return nextPos;
        }
        return leaf::new_error(reader_error::not_enough_buffer_size);
    }

    inline constexpr Src src() const noexcept
    {
        return Src(buf_.data() + pos_.byteIndex());
    }

    buf_view_const buf_;
    bit_pos pos_;
};

template <typename ImplT = Core>
result<bin_reader<ImplT>> make_bin_reader(buf_view_const aBufView,
                                          bit_pos aStartBit) noexcept
{
    BOOST_LEAF_CHECK(details::validate_args(aBufView, aStartBit));
    return bin_reader<ImplT>(aBufView, aStartBit);
}

template <typename ImplT = Core>
result<bin_reader<ImplT>> make_bin_reader(buf_view_const aBufView) noexcept
{
    return make_bin_reader<ImplT>(aBufView, bit_pos(0));
}

//
template <typename ImplT = Core, std::size_t N>
result<bin_reader<ImplT>> make_bin_reader(const uint8_t (&aData)[N],
                                          bit_pos aStartBit) noexcept
{
    BOOST_LEAF_AUTO(buf, buffer::make_bv_const(aData));
    return make_bin_reader<ImplT>(buf, aStartBit);
}

template <typename ImplT = Core, std::size_t N>
result<bin_reader<ImplT>> make_bin_reader(const uint8_t (&aData)[N]) noexcept
{
    return make_bin_reader<ImplT>(aData, bit_pos(0));
}

template <typename ImplT = Core>
result<bin_reader<ImplT>> make_bin_reader(Src aSrc, n_bytes aSize,
                                          bit_pos aStartBit) noexcept
{
    BOOST_LEAF_AUTO(buf, buffer::make_bv_const(aSrc.get(), aSize));
    return make_bin_reader<ImplT>(buf, aStartBit);
}

template <typename ImplT = Core>
result<bin_reader<ImplT>> make_bin_reader(Src aSrc, n_bytes aSize) noexcept
{
    return make_bin_reader<ImplT>(aSrc, aSize, bit_pos(0));
}

}  // namespace rabbit

#endif /* rabbit_bin_reader_h */
