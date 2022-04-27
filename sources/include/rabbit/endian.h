#ifndef rabbit_endian_h
#define rabbit_endian_h

#include <climits>
#include <cstdint>
#include <cstring>
#include <string_view>
#include <type_traits>
#include <utility>

namespace rabbit
{
enum class eEndian
{
    kLittle,
    kBig,
    kMixed
};

template <typename T>
struct is_uint
    : std::integral_constant<bool,
                             std::is_unsigned_v<T> && !std::is_same_v<T, bool>>
{
};

template <typename T>
inline constexpr bool is_uint_v = is_uint<T>::value;

#ifndef HOST_BIG_ENDIAN
constexpr eEndian kEndian = eEndian::kLittle;
constexpr std::string_view kEndianName = "kLittle";
#else
constexpr eEndian kEndian = eEndian::kBig;
constexpr std::string_view kEndianName = "kBig";
#endif

template <typename T = uint32_t>
typename std::enable_if_t<std::is_trivial_v<T> && std::is_standard_layout_v<T>,
                          eEndian>
endian() noexcept
{
    static_assert(sizeof(T) == 4, "sizeof(T) must be 4");
    constexpr uint32_t hostOrder = 0x1020304;
    T result;

    constexpr uint8_t bigOrderBytes[sizeof(T)]{1, 2, 3, 4};
    std::memcpy(&result, &bigOrderBytes, sizeof(T));
    if (hostOrder == result)
    {
        return eEndian::kBig;
    }

    constexpr uint8_t littleOrderBytes[sizeof(T)]{4, 3, 2, 1};
    std::memcpy(&result, &littleOrderBytes, sizeof(T));
    if (hostOrder == result)
    {
        return eEndian::kLittle;
    }
    return eEndian::kMixed;
}

template <typename StubT = void>
std::string_view endianName() noexcept
{
    using namespace std::literals;
    if (endian() == eEndian::kLittle)
    {
        return "kLittle"sv;
    }
    else if (endian() == eEndian::kBig)
    {
        return "kBig"sv;
    }
    else
    {
        return "kMixed"sv;
    }
}

namespace details
{
template <typename T, std::size_t... I>
constexpr decltype(auto) bswap_impl(T&& aValue, std::index_sequence<I...>)
{
    using U = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(is_uint_v<U>,
                  "U must be one of unsigned integer types and not <bool>.");
    return static_cast<U>(
        (... | static_cast<U>((0b11111111 & (aValue >> (I * CHAR_BIT)))
                              << ((sizeof(U) - 1 - I) * CHAR_BIT))));
}
}  // namespace details

template <typename T>
constexpr decltype(auto) bswap(T&& aValue)
{
    using U = std::remove_cv_t<std::remove_reference_t<T>>;
    static_assert(is_uint_v<U>,
                  "U must be one of unsigned integer types and not <bool>.");
    return details::bswap_impl(std::forward<T>(aValue),
                               std::make_index_sequence<sizeof(U)>{});
}

template <typename T>
constexpr decltype(auto) toNet(T&& aValue) noexcept
{
    static_assert((kEndian == eEndian::kLittle) || (kEndian == eEndian::kBig),
                  "Sorry but such endian is unsuppurted.");
    if constexpr (kEndian == eEndian::kLittle)
    {
        return bswap(std::forward<T>(aValue));
    }
    else
    {
        return std::forward<T>(aValue);
    }
}

template <typename T>
constexpr decltype(auto) toHost(T&& aValue) noexcept
{
    return toNet(std::forward<T>(aValue));
}
}  // namespace rabbit

#endif /* rabbit_endian_h */