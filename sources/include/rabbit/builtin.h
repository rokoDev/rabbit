#ifndef rabbit_builtin_h
#define rabbit_builtin_h

#include <interval/interval.h>
#include <utils/utils.h>

#include <type_traits>
#include <utility>

#include "deserialize.h"
#include "serialize.h"

#define DEFINE_ENUM_MIN_MAX(e, eMin, eMax)                        \
    template <>                                                   \
    struct enum_traits<e>                                         \
    {                                                             \
        static constexpr std::underlying_type_t<e> min() noexcept \
        {                                                         \
            return utils::to_underlying(e::eMin);                 \
        }                                                         \
        static constexpr std::underlying_type_t<e> max() noexcept \
        {                                                         \
            return utils::to_underlying(e::eMax);                 \
        }                                                         \
    };

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
using enable_if_enum_t =
    std::enable_if_t<std::is_enum_v<utils::remove_cvref_t<T>>, T>;

template <typename E>
struct enum_traits
{
    static constexpr std::underlying_type_t<
        std::enable_if_t<std::is_enum_v<E>, E>>
    min() noexcept;
    static constexpr std::underlying_type_t<
        std::enable_if_t<std::is_enum_v<E>, E>>
    max() noexcept;
};

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

namespace details
{
template <typename IntervalT>
struct Interval;

template <typename T, T MinV, T MaxV>
struct Interval<interval::Interval<interval::Min<MinV>, interval::Max<MaxV>>>
{
    using interval_type =
        interval::Interval<interval::Min<MinV>, interval::Max<MaxV>>;
    static constexpr std::size_t kNumBits =
        utils::bits_count(interval_type::kMaxIndex);
    using UIntT = utils::uint_from_nbits_t<kNumBits>;
    static result<void> serialize(writer &aWriter, T &&aValue) noexcept
    {
        switch (interval_type::location(aValue))
        {
            case interval::eIntervalLocation::kInside:
            {
                UIntT valueToSave =
                    static_cast<UIntT>(interval_type::indexOf(aValue));
                BOOST_LEAF_CHECK(
                    aWriter.addValue(valueToSave, NumBits(kNumBits)));
                return {};
            }
            case interval::eIntervalLocation::kAbove:
            {
                return leaf::new_error(writer_error::value_above_interval);
            }
            default:
            {
                return leaf::new_error(writer_error::value_below_interval);
            }
        }
    }

    static result<T> deserialize(reader &aReader) noexcept
    {
        BOOST_LEAF_AUTO(index, aReader.getValue<UIntT>(NumBits(kNumBits)));
        if (index <= interval_type::kMaxIndex)
        {
            return interval_type::valueAt(index);
        }
        else
        {
            return leaf::new_error(reader_error::invalid_interval_index);
        }
    }
};
}  // namespace details

template <typename T>
result<void> serialize(writer &aWriter, T &&aValue,
                       tag_t<enable_if_enum_t<T>>) noexcept
{
    using E = utils::remove_cvref_t<T>;
    using IntervalT = interval::Interval<interval::Min<enum_traits<E>::min()>,
                                         interval::Max<enum_traits<E>::max()>>;
    auto underlyingValue = utils::to_underlying(aValue);
    BOOST_LEAF_CHECK(details::Interval<IntervalT>::serialize(
        aWriter, std::move(underlyingValue)));
    return {};
}

template <typename T>
result<enable_if_enum_t<T>> deserialize(reader &aReader, tag_t<T>) noexcept
{
    using E = utils::remove_cvref_t<T>;
    using IntervalT = interval::Interval<interval::Min<enum_traits<E>::min()>,
                                         interval::Max<enum_traits<E>::max()>>;
    BOOST_LEAF_AUTO(underlyingValue,
                    details::Interval<IntervalT>::deserialize(aReader));
    E value = static_cast<E>(underlyingValue);
    return value;
}
}  // namespace rabbit

#endif /* rabbit_builtin_h */
