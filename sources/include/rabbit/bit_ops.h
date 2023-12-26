#ifndef rabbit_bit_ops_h
#define rabbit_bit_ops_h

#include <utils/utils.h>

#include <cassert>
#include <climits>

#include "typedefs.h"

namespace rabbit
{
namespace bit_ops
{
template <typename T>
inline constexpr T mask(Offset aOffset, NumBits aNumBits) noexcept;

template <typename T>
inline constexpr T mask(NumBits aNumBits, Offset aOffset) noexcept;

template <typename T>
inline constexpr T invert(T aValue) noexcept;

template <typename T>
inline constexpr T mask_right(Offset aOffset) noexcept;

template <typename T>
inline constexpr T mask_right(NumBits aNumBits) noexcept;

template <typename T>
inline constexpr T mask_left(Offset aOffset) noexcept;

template <typename T>
inline constexpr T mask_left(NumBits aNumBits) noexcept;

template <typename T>
inline constexpr T shift_left(T aValue, std::size_t aOffset) noexcept;

template <typename T>
inline constexpr T shift_right(T aValue, std::size_t aOffset) noexcept;

template <typename T>
inline constexpr T bit_or(T aValue1, T aValue2) noexcept;

template <typename T>
inline constexpr T bit_and(T aValue1, T aValue2) noexcept;

template <typename T>
inline constexpr T compose(T aDst, T aSrc, T aMask) noexcept;

template <typename T>
inline constexpr T mask(Offset aOffset, NumBits aNumBits) noexcept
{
    static_assert(utils::is_uint_or_byte_v<T>);
    assert(aNumBits + aOffset <= num_bits<T>);
    assert(aNumBits > 0);
    constexpr auto kAllSet = invert(T{});
    const Offset tmp{static_cast<Offset::value_type>(num_bits<T> - aNumBits)};
    const auto kShifted = shift_left(kAllSet, tmp);
    const auto kMask = shift_right(kShifted, aOffset);
    return kMask;
}

template <typename T>
inline constexpr T mask(NumBits aNumBits, Offset aOffset) noexcept
{
    static_assert(utils::is_uint_or_byte_v<T>);
    assert(aNumBits + aOffset <= num_bits<T>);
    assert(aNumBits > 0);
    constexpr auto kAllSet = invert(T{});
    const Offset tmp{static_cast<Offset::value_type>(num_bits<T> - aNumBits)};
    const auto kShifted = shift_right(kAllSet, tmp);
    const auto kMask = shift_left(kShifted, aOffset);
    return kMask;
}

template <typename T>
inline constexpr T invert(T aValue) noexcept
{
    static_assert(utils::is_uint_or_byte_v<T>);
    return static_cast<T>(~aValue);
}

template <typename T>
inline constexpr T mask_right(Offset aOffset) noexcept
{
    static_assert(utils::is_uint_or_byte_v<T>);
    assert(aOffset < num_bits<T>);
    return shift_right(invert(T{}), aOffset);
}

template <typename T>
inline constexpr T mask_right(NumBits aNumBits) noexcept
{
    Offset tmp{static_cast<Offset::value_type>(num_bits<T> - aNumBits)};
    return mask_right<T>(tmp);
}

template <typename T>
inline constexpr T mask_left(Offset aOffset) noexcept
{
    static_assert(utils::is_uint_or_byte_v<T>);
    assert(aOffset < num_bits<T>);
    return shift_left(invert(T{}), aOffset);
}

template <typename T>
inline constexpr T mask_left(NumBits aNumBits) noexcept
{
    return mask_left<T>(Offset{num_bits<T> - aNumBits});
}

template <typename T>
inline constexpr T shift_left(T aValue, std::size_t aOffset) noexcept
{
    static_assert(utils::is_uint_or_byte_v<T>);
    assert(aOffset < num_bits<T>);
    return static_cast<T>(aValue << aOffset);
}

template <typename T>
inline constexpr T shift_right(T aValue, std::size_t aOffset) noexcept
{
    static_assert(utils::is_uint_or_byte_v<T>);
    assert(aOffset < num_bits<T>);
    return static_cast<T>(aValue >> aOffset);
}

template <typename T>
inline constexpr T bit_or(T aValue1, T aValue2) noexcept
{
    static_assert(utils::is_uint_or_byte_v<T>);
    return aValue1 | aValue2;
}

template <typename T>
inline constexpr T bit_and(T aValue1, T aValue2) noexcept
{
    static_assert(utils::is_uint_or_byte_v<T>);
    return aValue1 & aValue2;
}

template <typename T>
inline constexpr T compose(T aDst, T aSrc, T aMask) noexcept
{
    static_assert(utils::is_uint_or_byte_v<T>);
    return bit_or(bit_and(aMask, aSrc), bit_and(invert(aMask), aDst));
}
}  // namespace bit_ops
}  // namespace rabbit
#endif /* rabbit_bit_ops_h */
