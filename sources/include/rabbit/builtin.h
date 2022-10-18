#ifndef rabbit_builtin_h
#define rabbit_builtin_h

#include <interval/interval.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include <type_traits>
#include <utility>
#include <valarray>
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

template <typename T, typename U = T>
using enable_if_u_int_t = std::enable_if_t<is_uint_v<T>, U>;

template <typename T>
inline constexpr bool is_int_v = std::is_signed_v<utils::remove_cvref_t<T>>
    &&std::is_integral_v<utils::remove_cvref_t<T>>;

template <typename T>
inline constexpr bool is_floating_v =
    std::is_same_v<utils::remove_cvref_t<T>, float> ||
    std::is_same_v<utils::remove_cvref_t<T>, double>;

template <typename T, typename U = T>
using enable_if_i_or_f_t = std::enable_if_t<is_int_v<T> || is_floating_v<T>, U>;

template <typename T>
inline constexpr bool is_bool_v =
    std::is_same_v<utils::remove_cvref_t<T>, bool>;

template <typename T, typename U = T>
using enable_if_bool_t = std::enable_if_t<is_bool_v<T>, U>;

template <typename T, typename U = T>
using enable_if_enum_t =
    std::enable_if_t<std::is_enum_v<utils::remove_cvref_t<T>>, U>;

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

template <typename T, typename U = T>
using enable_if_vector_t =
    std::enable_if_t<is_vector_v<utils::remove_cvref_t<T>>, U>;

template <typename T>
struct is_valarray : std::false_type
{
};

template <typename T>
struct is_valarray<std::valarray<T>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_valarray_v = is_valarray<T>::value;

template <typename T, typename U = T>
using enable_if_valarray_t =
    std::enable_if_t<is_valarray_v<utils::remove_cvref_t<T>>, U>;

template <typename T>
struct is_std_string : std::false_type
{
};

template <>
struct is_std_string<std::string> : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_string_v = is_std_string<T>::value;

template <typename T, typename U = T>
using enable_if_std_string_t =
    std::enable_if_t<is_std_string_v<utils::remove_cvref_t<T>>, U>;

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
void serialize(simple_writer &aWriter, T &aValue,
               tag_t<enable_if_u_int_t<T>>) noexcept
{
    aWriter.addValue(aValue);
}

template <typename T>
void serialize(simple_writer &aWriter, T &aValue,
               tag_t<enable_if_i_or_f_t<T>>) noexcept
{
    using CoreT = utils::remove_cvref_t<T>;
    using UIntT = utils::UInt<sizeof(CoreT)>;
    UIntT uValue = utils::bit_cast<UIntT>(aValue);
    aWriter.addValue(std::move(uValue));
}

template <typename T>
void serialize(simple_writer &aWriter, T &aValue,
               tag_t<enable_if_bool_t<T>>) noexcept
{
    uint8_t uValue = static_cast<uint8_t>(aValue);
    aWriter.addValue(std::move(uValue), NumBits(1));
}

template <typename T>
result<void> serialize(writer &aWriter, T &&aValue,
                       tag_t<enable_if_u_int_t<T>>) noexcept
{
    BOOST_LEAF_CHECK(aWriter.addValue(aValue));
    return {};
}

template <typename T>
enable_if_u_int_t<T, eReaderError> deserialize(simple_reader &aReader,
                                               T &aValue, tag_t<T>) noexcept
{
    return aReader.getValue(aValue);
}

template <typename T>
enable_if_i_or_f_t<T, eReaderError> deserialize(simple_reader &aReader,
                                                T &aValue, tag_t<T>) noexcept
{
    using UIntT = utils::UInt<sizeof(T)>;
    UIntT uValue{};
    const auto retVal = aReader.getValue(uValue);
    if (retVal == eReaderError::kSuccess)
    {
        aValue = utils::bit_cast<T>(uValue);
    }
    return retVal;
}

template <typename T>
enable_if_bool_t<T, eReaderError> deserialize(simple_reader &aReader, T &aValue,
                                              tag_t<T>) noexcept
{
    uint8_t interimValue{};
    const auto retVal = aReader.getValue(interimValue, NumBits(1));
    aValue = static_cast<bool>(interimValue);
    return retVal;
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

    static constexpr void serialize(simple_writer &aWriter, T aValue) noexcept
    {
        UIntT valueToSave = static_cast<UIntT>(interval_type::indexOf(aValue));
        aWriter.addValue(valueToSave, NumBits(kNumBits));
    }

    template <typename E>
    static eReaderError deserialize(simple_reader &aReader, E &aValue) noexcept
    {
        UIntT index{};
        auto retVal = aReader.getValue(index, NumBits(kNumBits));
        if (retVal == eReaderError::kSuccess)
        {
            if (index <= interval_type::kMaxIndex)
            {
                aValue = static_cast<E>(interval_type::valueAt(index));
            }
            else
            {
                retVal = eReaderError::kInvalidEnumValue;
            }
        }
        return retVal;
    }

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
constexpr void serialize(simple_writer &aWriter, T &&aValue,
                         tag_t<enable_if_enum_t<T>>) noexcept
{
    using E = utils::remove_cvref_t<T>;
    using IntervalT = interval::Interval<interval::Min<enum_traits<E>::min()>,
                                         interval::Max<enum_traits<E>::max()>>;
    auto underlyingValue = utils::to_underlying(aValue);
    details::Interval<IntervalT>::serialize(aWriter,
                                            std::move(underlyingValue));
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
enable_if_enum_t<T, eReaderError> deserialize(simple_reader &aReader, T &aValue,
                                              tag_t<T>) noexcept
{
    using IntervalT = interval::Interval<interval::Min<enum_traits<T>::min()>,
                                         interval::Max<enum_traits<T>::max()>>;
    return details::Interval<IntervalT>::deserialize(aReader, aValue);
}

namespace details
{
template <typename T>
void serialize_vector_like(simple_writer &aWriter, T &aValue) noexcept
{
    using VectorT = utils::remove_cvref_t<T>;
    using ValueT = typename VectorT::value_type;
    const std::size_t kSize = aValue.size();
    constexpr uint32_t kMaxSize =
        static_cast<uint32_t>(std::numeric_limits<uint32_t>::max() >> 1);
    if (kSize)
    {
        const uint32_t kSize32 = static_cast<uint32_t>(kSize);
        const uint32_t kNonEmptyFlag = static_cast<uint32_t>(~kMaxSize);
        const uint32_t kValueToSave =
            static_cast<uint32_t>(kSize32 | kNonEmptyFlag);
        aWriter.addValue(std::move(kValueToSave));
        if constexpr ((std::alignment_of_v<ValueT> ==
                       std::alignment_of_v<uint8_t>)&&(sizeof(ValueT) ==
                                                       sizeof(uint8_t)))
        {
            uint8_t const *firstPtr =
                reinterpret_cast<uint8_t const *>(&(aValue[0]));
            aWriter.addBits(Src(firstPtr), NumBits(kSize * CHAR_BIT));
        }
        else
        {
            for (const auto &instance: aValue)
            {
                serialize(aWriter, instance);
            }
        }
    }
    else
    {
        aWriter.addValue(0_u8, NumBits(1));
    }
}

template <typename T>
eReaderError deserialize_vector_like(simple_reader &aReader, T &aValue) noexcept
{
    using VectorT = T;
    using ValueT = typename VectorT::value_type;
    uint8_t nonEmptyFlag{};
    auto retVal = aReader.getValue(nonEmptyFlag, NumBits(1));
    if (retVal == eReaderError::kSuccess)
    {
        if (nonEmptyFlag)
        {
            uint32_t kSize{};
            retVal = aReader.getValue(kSize, NumBits(31));
            if (retVal == eReaderError::kSuccess)
            {
                if (kSize > 0)
                {
                    if constexpr ((std::alignment_of_v<ValueT> ==
                                   std::alignment_of_v<
                                       uint8_t>)&&(sizeof(ValueT) ==
                                                   sizeof(uint8_t)))
                    {
                        aValue.resize(kSize);
                        retVal = aReader.getBits(
                            Dst(reinterpret_cast<uint8_t *>(aValue.data())),
                            DstBitOffset(0), NumBits(kSize * CHAR_BIT));
                    }
                    else
                    {
                        aValue.resize(kSize);
                        for (auto &element: aValue)
                        {
                            retVal = deserialize(aReader, element);
                            if (retVal != eReaderError::kSuccess)
                            {
                                break;
                            }
                        }
                    }
                }
                else
                {
                    retVal = eReaderError::kNonEmptyContainerSizeIsZero;
                }
            }
        }
        else
        {
            aValue.resize(0_uz);
        }
    }
    return retVal;
}
}  // namespace details

template <typename T>
void serialize(simple_writer &aWriter, T &aValue,
               tag_t<enable_if_vector_t<T>>) noexcept
{
    details::serialize_vector_like(aWriter, aValue);
}

template <typename T>
void serialize(simple_writer &aWriter, T &aValue,
               tag_t<enable_if_valarray_t<T>>) noexcept
{
    details::serialize_vector_like(aWriter, aValue);
}

template <typename T>
void serialize(simple_writer &aWriter, T &aValue,
               tag_t<enable_if_std_string_t<T>>) noexcept
{
    details::serialize_vector_like(aWriter, aValue);
}

template <typename T>
enable_if_vector_t<T, eReaderError> deserialize(simple_reader &aReader,
                                                T &aValue, tag_t<T>)
{
    return details::deserialize_vector_like(aReader, aValue);
}

template <typename T>
enable_if_valarray_t<T, eReaderError> deserialize(simple_reader &aReader,
                                                  T &aValue, tag_t<T>)
{
    return details::deserialize_vector_like(aReader, aValue);
}

template <typename T>
enable_if_std_string_t<T, eReaderError> deserialize(simple_reader &aReader,
                                                    T &aValue, tag_t<T>)
{
    return details::deserialize_vector_like(aReader, aValue);
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

template <typename T>
result<void> serialize(writer &aWriter, T &&aValue,
                       tag_t<enable_if_valarray_t<T>>) noexcept
{
    using ValarrayT = utils::remove_cvref_t<T>;
    using ValueT = typename ValarrayT::value_type;
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
            uint8_t const *firstPtr =
                reinterpret_cast<uint8_t const *>(&(aValue[0]));
            BOOST_LEAF_CHECK(
                aWriter.addBits(Src(firstPtr), NumBits(kSize * CHAR_BIT)));
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
result<enable_if_valarray_t<T>> deserialize(reader &aReader, tag_t<T>)
{
    using ValarrayT = utils::remove_cvref_t<T>;
    using ValueT = typename ValarrayT::value_type;
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
                ValarrayT vec(kSize);
                uint8_t *firstPtr = reinterpret_cast<uint8_t *>(&(vec[0]));
                BOOST_LEAF_CHECK(aReader.getBits(Dst(firstPtr), DstBitOffset(0),
                                                 NumBits(kSize * CHAR_BIT)));
                return vec;
            }
            else
            {
                ValarrayT vec(kSize);
                for (auto &value: vec)
                {
                    BOOST_LEAF_ASSIGN(value, deserialize<ValueT>(aReader));
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
        return ValarrayT{};
    }
}

template <typename T>
result<void> serialize(writer &aWriter, T &&aValue,
                       tag_t<enable_if_std_string_t<T>>) noexcept
{
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
        BOOST_LEAF_CHECK(aWriter.addBits(
            Src(reinterpret_cast<uint8_t const *>(aValue.data())),
            NumBits(kSize * CHAR_BIT)));
    }
    else
    {
        BOOST_LEAF_CHECK(aWriter.addValue(0_u8, NumBits(1)));
    }
    return {};
}

template <typename T>
result<enable_if_std_string_t<T>> deserialize(reader &aReader, tag_t<T>)
{
    using StrT = utils::remove_cvref_t<T>;
    BOOST_LEAF_AUTO(nonEmptyFlag, aReader.getValue<uint8_t>(NumBits(1)));
    if (nonEmptyFlag)
    {
        BOOST_LEAF_AUTO(kSize, aReader.getValue<uint32_t>(NumBits(31)));
        if (kSize > 0)
        {
            StrT str(kSize, ' ');
            BOOST_LEAF_CHECK(
                aReader.getBits(Dst(reinterpret_cast<uint8_t *>(str.data())),
                                DstBitOffset(0), NumBits(kSize * CHAR_BIT)));
            return str;
        }
        else
        {
            return leaf::new_error(reader_error::non_empty_vector_size_is_zero);
        }
    }
    else
    {
        return StrT{};
    }
}

template <typename T>
constexpr std::enable_if_t<std::is_arithmetic_v<T>, std::size_t> bit_size(
    tag_t<T>) noexcept
{
    return utils::num_bits<T>();
};

template <typename T>
constexpr std::enable_if_t<std::is_enum_v<T>, std::size_t> bit_size(
    tag_t<T>) noexcept
{
    using IntervalT = interval::Interval<interval::Min<enum_traits<T>::min()>,
                                         interval::Max<enum_traits<T>::max()>>;
    return details::Interval<IntervalT>::kNumBits;
};

namespace details
{
template <typename T>
static constexpr std::size_t vector_like_bit_size(const T &aValue) noexcept
{
    using ValueT = typename T::value_type;
    const auto kSize = aValue.size();
    if (kSize)
    {
        if constexpr (is_compile_time_computable_size_v<ValueT>)
        {
            return utils::num_bits<uint32_t>() + kSize * bit_sizeof_v<ValueT>;
        }
        else
        {
            std::size_t retVal{utils::num_bits<uint32_t>()};
            for (const auto &element: aValue)
            {
                retVal += bit_sizeof(element);
            }
            return retVal;
        }
    }
    else
    {
        return 1_uz;
    }
}
}  // namespace details

template <typename T>
class SizeChecker<std::vector<T>>
{
   public:
    static std::size_t bit_size(const std::vector<T> &aValue) noexcept
    {
        return details::vector_like_bit_size(aValue);
    }
};

template <typename T>
class SizeChecker<std::valarray<T>>
{
   public:
    static std::size_t bit_size(const std::valarray<T> &aValue) noexcept
    {
        return details::vector_like_bit_size(aValue);
    }
};

template <>
class SizeChecker<std::string>
{
   public:
    static std::size_t bit_size(const std::string &aValue) noexcept
    {
        return details::vector_like_bit_size(aValue);
    }
};
}  // namespace rabbit

#endif /* rabbit_builtin_h */
