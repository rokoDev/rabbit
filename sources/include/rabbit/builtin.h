#ifndef rabbit_builtin_h
#define rabbit_builtin_h

#include <utils/utils.h>

#include <type_traits>
#include <utility>

#include "deserialize.h"
#include "serialize.h"

namespace rabbit
{
template <typename T>
inline constexpr bool is_uint_v =
    std::is_unsigned_v<utils::remove_cvref_t<T>> &&
    (not std::is_same_v<utils::remove_cvref_t<T>, bool>);

template <typename T>
using enable_if_u_int_t = std::enable_if_t<is_uint_v<T>, T>;

template <typename T>
inline constexpr bool is_int_v = std::is_signed_v<utils::remove_cvref_t<T>>
    &&std::is_integral_v<utils::remove_cvref_t<T>>;

template <typename T>
inline constexpr bool is_floating_v =
    std::is_same_v<utils::remove_cvref_t<T>, float> ||
    std::is_same_v<utils::remove_cvref_t<T>, double>;

template <typename T>
using enable_if_i_or_f_t = std::enable_if_t<is_int_v<T> || is_floating_v<T>, T>;

template <typename T>
inline constexpr bool is_bool_v =
    std::is_same_v<utils::remove_cvref_t<T>, bool>;

template <typename T>
using enable_if_bool_t = std::enable_if_t<is_bool_v<T>, T>;

template <typename T>
struct is_u_enum
    : std::conditional_t<std::is_enum_v<utils::remove_cvref_t<T>>,
                         std::true_type, std::false_type>
{
};

template <typename T>
inline constexpr bool is_u_enum_v = is_u_enum<T>::value;

template <typename T>
using enable_if_u_enum_t = std::enable_if_t<is_u_enum_v<T>, T>;

template <typename T>
result<void> serialize(writer &aWriter, T &&aValue,
                       tag_t<enable_if_u_int_t<T>>) noexcept
{
    BOOST_LEAF_CHECK(aWriter.addValue(aValue));
    return {};
}

template <typename T>
result<enable_if_u_int_t<T>> deserialize(reader &aReader, tag_t<T>) noexcept
{
    BOOST_LEAF_AUTO(r, aReader.getValue<T>());
    return r;
}

template <typename T>
result<void> serialize(writer &aWriter, T &&aValue,
                       tag_t<enable_if_i_or_f_t<T>>) noexcept
{
    using CoreT = utils::remove_cvref_t<T>;
    using UIntT = utils::UInt<sizeof(CoreT)>;
    UIntT uValue = utils::bit_cast<UIntT>(aValue);
    BOOST_LEAF_CHECK(aWriter.addValue(std::move(uValue)));
    return {};
}

template <typename T>
result<enable_if_i_or_f_t<T>> deserialize(reader &aReader, tag_t<T>) noexcept
{
    using CoreT = utils::remove_cvref_t<T>;
    using UIntT = utils::UInt<sizeof(CoreT)>;
    BOOST_LEAF_AUTO(uValue, aReader.getValue<UIntT>());
    CoreT value = utils::bit_cast<CoreT>(uValue);
    return value;
}

template <typename T>
result<void> serialize(writer &aWriter, T &&aValue,
                       tag_t<enable_if_bool_t<T>>) noexcept
{
    uint8_t uValue = static_cast<uint8_t>(aValue);
    BOOST_LEAF_CHECK(aWriter.addValue(std::move(uValue), NumBits(1)));
    return {};
}

template <typename T>
result<enable_if_bool_t<T>> deserialize(reader &aReader, tag_t<T>) noexcept
{
    BOOST_LEAF_AUTO(uValue, aReader.getValue<uint8_t>(NumBits(1)));
    bool value = static_cast<bool>(uValue);
    return value;
}

template <typename T>
result<void> serialize(writer &aWriter, T &&aValue,
                       tag_t<enable_if_u_enum_t<T>>) noexcept
{
    using E = utils::remove_cvref_t<T>;
    using UIntT = typename utils::enum_properties<E>::SerializeT;
    UIntT uValue = static_cast<UIntT>(aValue);
    if (uValue >= static_cast<UIntT>(E::Count))
    {
        return leaf::new_error(writer_error::invalid_enum_value);
    }
    BOOST_LEAF_CHECK(aWriter.addValue(
        std::move(uValue), NumBits(utils::enum_properties<E>::numBits)));
    return {};
}

template <typename T>
result<enable_if_u_enum_t<T>> deserialize(reader &aReader, tag_t<T>) noexcept
{
    using E = utils::remove_cvref_t<T>;
    using UIntT = typename utils::enum_properties<E>::SerializeT;
    BOOST_LEAF_AUTO(uValue, aReader.getValue<UIntT>(
                                NumBits(utils::enum_properties<E>::numBits)));
    if (uValue >= static_cast<UIntT>(E::Count))
    {
        return leaf::new_error(reader_error::invalid_enum_value);
    }
    E value = static_cast<E>(uValue);
    return value;
}
}  // namespace rabbit

#endif /* rabbit_builtin_h */
