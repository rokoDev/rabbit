#ifndef rabbit_builtin_h
#define rabbit_builtin_h

#include <interval/interval.h>
#include <utils/utils.h>

#include <type_traits>
#include <utility>
#include <vector>

#include "deserialize.h"
#include "serialize.h"

#define RABBIT_ENUM_MIN_MAX(e, eMin, eMax)                        \
    namespace rabbit                                              \
    {                                                             \
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
    };                                                            \
    }  // namespace rabbit

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

template <typename T>
struct is_vector : std::false_type
{
};

template <typename T>
struct is_vector<std::vector<T>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_vector_v = is_vector<T>::value;

template <typename T>
using enable_if_vector_t =
    std::enable_if_t<is_vector_v<utils::remove_cvref_t<T>>, T>;

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

template <typename T>
result<void> serialize(writer &aWriter, T &&aValue,
                       tag_t<enable_if_vector_t<T>>) noexcept
{
    using VectorT = utils::remove_cvref_t<T>;
    using ValueT = typename VectorT::value_type;
    const std::size_t kSize = aValue.size();
    constexpr uint32_t kMaxSize =
        static_cast<uint32_t>(std::numeric_limits<uint32_t>::max() >> 1);
    if (kSize > kMaxSize)
    {
        return leaf::new_error(writer_error::vector_size_is_too_big);
    }
    if (kSize)
    {
        const uint32_t kSize32 = static_cast<uint32_t>(kSize);
        const uint32_t kNonEmptyFlag = static_cast<uint32_t>(~kMaxSize);
        const uint32_t kValueToSave =
            static_cast<uint32_t>(kSize32 | kNonEmptyFlag);
        BOOST_LEAF_CHECK(aWriter.addValue(std::move(kValueToSave)));
        if constexpr ((std::alignment_of_v<ValueT> ==
                       std::alignment_of_v<uint8_t>)&&(sizeof(ValueT) ==
                                                       sizeof(uint8_t)))
        {
            BOOST_LEAF_CHECK(aWriter.addBits(
                Src(reinterpret_cast<uint8_t const *>(aValue.data())),
                NumBits(kSize * CHAR_BIT)));
        }
        else
        {
            for (const auto &instance: aValue)
            {
                BOOST_LEAF_CHECK(serialize(aWriter, instance));
            }
        }
    }
    else
    {
        BOOST_LEAF_CHECK(aWriter.addValue(0_u8, NumBits(1)));
    }
    return {};
}

template <typename T>
result<enable_if_vector_t<T>> deserialize(reader &aReader, tag_t<T>)
{
    using VectorT = utils::remove_cvref_t<T>;
    using ValueT = typename VectorT::value_type;
    BOOST_LEAF_AUTO(nonEmptyFlag, aReader.getValue<uint8_t>(NumBits(1)));
    if (nonEmptyFlag)
    {
        BOOST_LEAF_AUTO(kSize, aReader.getValue<uint32_t>(NumBits(31)));
        if (kSize > 0)
        {
            if constexpr ((std::alignment_of_v<ValueT> ==
                           std::alignment_of_v<uint8_t>)&&(sizeof(ValueT) ==
                                                           sizeof(uint8_t)))
            {
                VectorT vec(kSize);
                BOOST_LEAF_CHECK(aReader.getBits(
                    Dst(reinterpret_cast<uint8_t *>(vec.data())),
                    DstBitOffset(0), NumBits(kSize * CHAR_BIT)));
                return vec;
            }
            else
            {
                VectorT vec{};
                vec.reserve(kSize);
                for (std::size_t i = 0; i < kSize; ++i)
                {
                    BOOST_LEAF_AUTO(value, deserialize<ValueT>(aReader));
                    vec.push_back(value);
                }
                return vec;
            }
        }
        else
        {
            return leaf::new_error(reader_error::non_empty_vector_size_is_zero);
        }
    }
    else
    {
        return VectorT{};
    }
}
}  // namespace rabbit

#endif /* rabbit_builtin_h */