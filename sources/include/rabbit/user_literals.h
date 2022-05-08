#ifndef rabbit_user_literals_h
#define rabbit_user_literals_h

#include <cstddef>
#include <cstdint>

constexpr std::size_t operator"" _uz(unsigned long long v)
{
    return static_cast<std::size_t>(v);
}

constexpr std::int8_t operator"" _i8(unsigned long long v)
{
    return static_cast<std::int8_t>(v);
}

constexpr std::uint8_t operator"" _u8(unsigned long long v)
{
    return static_cast<std::uint8_t>(v);
}

constexpr std::int16_t operator"" _i16(unsigned long long v)
{
    return static_cast<std::int16_t>(v);
}

constexpr std::uint16_t operator"" _u16(unsigned long long v)
{
    return static_cast<std::uint16_t>(v);
}

constexpr std::int32_t operator"" _i32(unsigned long long v)
{
    return static_cast<std::int32_t>(v);
}

constexpr std::uint32_t operator"" _u32(unsigned long long v)
{
    return static_cast<std::uint32_t>(v);
}

constexpr std::int64_t operator"" _i64(unsigned long long v)
{
    return static_cast<std::int64_t>(v);
}

constexpr std::uint64_t operator"" _u64(unsigned long long v)
{
    return static_cast<std::uint64_t>(v);
}

constexpr std::int_fast8_t operator"" _if8(unsigned long long v)
{
    return static_cast<std::int_fast8_t>(v);
}

constexpr std::uint_fast8_t operator"" _uf8(unsigned long long v)
{
    return static_cast<std::uint_fast8_t>(v);
}

constexpr std::int_fast16_t operator"" _if16(unsigned long long v)
{
    return static_cast<std::int_fast16_t>(v);
}

constexpr std::uint_fast16_t operator"" _uf16(unsigned long long v)
{
    return static_cast<std::uint_fast16_t>(v);
}

constexpr std::int_fast32_t operator"" _if32(unsigned long long v)
{
    return static_cast<std::int_fast32_t>(v);
}

constexpr std::uint_fast32_t operator"" _uf32(unsigned long long v)
{
    return static_cast<std::uint_fast32_t>(v);
}

constexpr std::int_fast64_t operator"" _if64(unsigned long long v)
{
    return static_cast<std::int_fast64_t>(v);
}

constexpr std::uint_fast64_t operator"" _uf64(unsigned long long v)
{
    return static_cast<std::uint_fast64_t>(v);
}

constexpr std::intmax_t operator"" _imax(unsigned long long v)
{
    return static_cast<std::intmax_t>(v);
}

constexpr std::uintmax_t operator"" _umax(unsigned long long v)
{
    return static_cast<std::uintmax_t>(v);
}

constexpr std::intptr_t operator"" _intptr(unsigned long long v)
{
    return static_cast<std::intptr_t>(v);
}

constexpr std::uintptr_t operator"" _uintptr(unsigned long long v)
{
    return static_cast<std::uintptr_t>(v);
}

#endif /* rabbit_user_literals_h */
