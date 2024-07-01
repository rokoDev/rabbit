#ifndef rabbit_bit_holder_h
#define rabbit_bit_holder_h

namespace rabbit
{
CREATE_MEMBER_TYPE_CHECKERS(value_type)

namespace details
{
template <typename BufView, template <typename> class Tag,
          typename ResultAdapter>
class bit_holder
{
   public:
    using value_type = std::byte;
    using buffer_view = BufView;

    template <typename T>
    static constexpr std::size_t size_by_type = size_defined_by_type_v<T, Tag>;

    template <typename T>
    using tag_t = Tag<T>;

    using result_adapter = ResultAdapter;
    using result = typename ResultAdapter::result;

    bit_holder() = delete;

    inline constexpr bit_holder(buffer_view aBufView, bit_pos aStartBit,
                                std::size_t aBitsTaken = 0) noexcept
        : buf_{aBufView}, pos_{aStartBit}, bits_taken_{aBitsTaken}
    {
        assert(aStartBit <= aBufView.size() && "Invalid aStartBit");
        assert(aBitsTaken <= aBufView.size() && "Invalid aBitsTaken");
    }

    inline constexpr decltype(auto) operator[](n_bytes aIndex) noexcept
    {
        return buf_[aIndex];
    }

    inline constexpr std::uint_fast8_t bit_offset() const noexcept
    {
        return pos_.bitOffset();
    }

    inline constexpr buffer_view buffer() const noexcept { return buf_; }

    inline constexpr bit_pos pos() const noexcept { return pos_; }

    inline constexpr n_bytes byte_index() const noexcept
    {
        return n_bytes{pos_.byteIndex()};
    }

    inline constexpr std::size_t bytes_used() const noexcept
    {
        return pos_.bytesUsed();
    }

    inline constexpr std::size_t bits_left() const noexcept
    {
        assert(pos_.get() <= bits_size());
        return bits_size() - pos_.get();
    }

    inline constexpr std::size_t bits_size() const noexcept
    {
        return buf_.bit_size().get();
    }

    inline constexpr std::size_t bits_taken() const noexcept
    {
        return bits_taken_;
    }

    inline constexpr void take_bits(std::size_t aNumBits) noexcept
    {
        bits_taken_ += aNumBits;
    }

    template <typename... Args>
    static constexpr decltype(auto) new_error(Args &&...aArgs) noexcept
    {
        return result_adapter::base::new_error(std::forward<Args>(aArgs)...);
    }

    static constexpr decltype(auto) success() noexcept
    {
        return result_adapter::base::success();
    }

    template <typename R>
    static constexpr bool is_success(R &&aResult) noexcept
    {
        return result_adapter::base::is_success(std::forward<R>(aResult));
    }

    template <typename R>
    static constexpr bool is_error(R &&aResult) noexcept
    {
        return result_adapter::base::is_error(std::forward<R>(aResult));
    }

   protected:
    constexpr void inc_pos(NumBits aNBits) noexcept { pos_ += bit_pos(aNBits); }

    buffer_view buf_;
    bit_pos pos_;
    std::size_t bits_taken_;
};

template <typename Container, auto ErrorValue, typename Bitholder,
          typename SizeType,
          typename = std::enable_if_t<has_type_value_type_v<Container>>>
decltype(auto) take_minimum_bits_for_container_elements(
    Bitholder &aBitholder, SizeType aNumElements) noexcept
{
    using value_type = typename Container::value_type;

    const std::size_t min_bit_size_of_elements =
        size_defined_by_type_v<value_type, Bitholder::template tag_t> *
        aNumElements;
    if (aBitholder.bits_taken() + min_bit_size_of_elements >
        aBitholder.bits_size())
    {  // not taken bits are not enough to store container' elements
        return Bitholder::new_error(ErrorValue);
    }
    aBitholder.take_bits(min_bit_size_of_elements);
    return Bitholder::success();
}
}  // namespace details
}  // namespace rabbit

#endif /* rabbit_bit_holder_h */
