#ifndef rabbit_bitset_h
#define rabbit_bitset_h

#include <strong_type/strong_type.h>

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

namespace impl
{
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
class Index;

template <>
class Index<eEndian::kLittle> : public IndexBase<Index<eEndian::kLittle>>
{
    friend class IndexBase<Index<eEndian::kLittle>>;

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
class Index<eEndian::kBig> : public IndexBase<Index<eEndian::kBig>>
{
    friend class IndexBase<Index<eEndian::kBig>>;

   private:
    template <typename T>
    static constexpr std::size_t getImpl(std::size_t I) noexcept
    {
        assert(I < sizeof(T) && "Invalid I");
        return I;
    }
};

template <typename T, std::size_t I>
constexpr std::size_t CIndex = Index<kEndian>::get<T, I>();

template <typename T>
constexpr std::size_t Index(std::size_t I) noexcept
{
    return Index<kEndian>::get<T>(I);
}
}  // namespace impl

class BinOps
{
   public:
    template <typename T>
    static constexpr void addValue(uint8_t *aDst, DstBitOffset aOffset,
                                   T &&aValue, NumBits aNBits) noexcept;

    template <typename T>
    static constexpr void addValue(uint8_t *aDst, T &&aValue,
                                   NumBits aNBits) noexcept;

    template <typename T>
    static constexpr void addValue(uint8_t *aDst, T &&aValue) noexcept;

    static constexpr void addBits(uint8_t *aDst, DstBitOffset aDstOffset,
                                  uint8_t const *aSrc, SrcBitOffset aSrcOffset,
                                  NumBits aNBits) noexcept;

    static constexpr void addBits(uint8_t *aDst, uint8_t const *aSrc,
                                  BitOffset aOffset, NumBits aNBits) noexcept;

    static constexpr void addBits(uint8_t *aDst, uint8_t const *aSrc,
                                  NumBits aNBits) noexcept;
};
}  // namespace rabbit

#endif /* rabbit_bitset_h */