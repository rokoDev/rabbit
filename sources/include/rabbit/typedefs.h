#ifndef rabbit_typedefs_h
#define rabbit_typedefs_h

#include <buffer/buffer.h>
#include <strong_type/strong_type.h>

#include <boost/leaf.hpp>
#include <boost/pfr.hpp>
#include <cstddef>
#include <cstdint>

namespace rabbit
{
namespace pfr = boost::pfr;
namespace leaf = boost::leaf;

template <class T>
using result = leaf::result<T>;

template <typename T>
struct tag_t
{
};

template <typename T>
constexpr tag_t<T> tag{};

template <typename StrongT>
struct NecessaryOps
    : strong::plus<StrongT>
    , strong::plus_assignment<StrongT>
    , strong::minus<StrongT>
    , strong::minus_assignment<StrongT>
    , strong::convertible_to_bool<StrongT>
    , strong::modulo<StrongT>
    , strong::comparisons<StrongT>
    , strong::implicitly_convertible_to_underlying<StrongT>
{
};

using SrcOffset =
    strong::strong_type<struct SrcOffsetTag, std::uint_fast8_t, NecessaryOps>;
using DstOffset =
    strong::strong_type<struct DstOffsetTag, std::uint_fast8_t, NecessaryOps>;
using Offset =
    strong::strong_type<struct OffsetTag, std::uint_fast8_t, NecessaryOps>;
using NumBits = buffer::n_bits;
using BitIndex =
    strong::strong_type<struct BitIndexTag, std::size_t, NecessaryOps>;

template <typename T>
inline constexpr NumBits num_bits{sizeof(T) * CHAR_BIT};

template <typename StrongT>
struct DataPtrOps
    : strong::indirection<StrongT>
    , strong::subscription<StrongT>
    , strong::comparisons<StrongT>
    , strong::implicitly_convertible_to_underlying<StrongT>
    , strong::pointer_plus_value<StrongT>
    , strong::pointer_plus_assignment<StrongT>
    , strong::pre_increment<StrongT>
{
};

using Src = strong::strong_type<struct SrcTag, std::byte const *, DataPtrOps>;
using Dst = strong::strong_type<struct DstTag, std::byte *, DataPtrOps>;

using simple_buf_view = buffer::simple_buffer_view<std::byte>;
using simple_buf_view_const =
    buffer::simple_buffer_view_const<simple_buf_view::value_type>;
using buf_view = buffer::buffer_view<std::byte>;
using buf_view_const = buffer::buffer_view_const<buf_view::value_type>;
using bit_pos = buffer::bit_pos;
using n_bytes = buffer::n_bytes;

enum class eReaderError
{
    kSuccess = 0,
    kNotEnoughBufferSize,
    kInvalidEnumValue,
    kNonEmptyContainerSizeIsZero,
};

inline namespace v1
{
class Core;
}

namespace v2
{
class Core;
}

template <typename ImplT>
class simple_bin_writer;

using simple_writer = simple_bin_writer<Core>;

template <typename ImplT>
class simple_bin_reader;

using simple_reader = simple_bin_reader<Core>;

template <typename ImplT>
class bin_writer;

using writer = bin_writer<Core>;

template <typename ImplT>
class bin_reader;

using reader = bin_reader<Core>;

template <typename T>
void serialize(simple_writer &aWriter, T &aValue) noexcept;

template <typename T>
eReaderError deserialize(simple_reader &aReader, T &aValue) noexcept;

template <typename T>
result<void> serialize(writer &aWriter, T &&aValue) noexcept;

template <typename T>
result<void> deserialize(reader &aReader, T &aValue) noexcept;

template <typename T>
class SizeChecker
{
    static constexpr void bit_size(const T &aValue) noexcept;
};

namespace details
{
template <class T>
using tagged_simple_serialize_result_t = decltype(serialize(
    std::declval<simple_writer &>(), std::declval<const T &>(), tag<T>));

template <class T>
using tagged_simple_deserialize_result_t = decltype(deserialize(
    std::declval<simple_reader &>(), std::declval<T &>(), tag<T>));

template <class T>
using tagged_serialize_result_t =
    decltype(serialize(std::declval<writer &>(), std::declval<T>(), tag<T>));

template <class T>
using tagged_deserialize_result_t = decltype(deserialize(
    std::declval<reader &>(), std::declval<T &>(), tag<T>));

template <class T>
using tagged_bit_size_result_t = decltype(bit_size(tag<T>));

template <typename T>
inline constexpr bool is_tagged_bit_size_defined_v =
    utils::is_detected_exact_v<std::size_t, tagged_bit_size_result_t, T>;

template <typename T>
using bit_size_result_t =
    decltype(SizeChecker<T>::bit_size(std::declval<const T &>()));
}  // namespace details

template <typename T>
inline constexpr bool is_simple_serialize_defined_v =
    utils::is_detected_exact_v<void, details::tagged_simple_serialize_result_t,
                               T>;

template <typename T>
inline constexpr bool is_simple_deserialize_defined_v =
    utils::is_detected_exact_v<eReaderError,
                               details::tagged_simple_deserialize_result_t, T>;

template <typename T>
inline constexpr bool is_serialize_defined_v =
    utils::is_detected_exact_v<result<void>, details::tagged_serialize_result_t,
                               T>;

template <typename T>
inline constexpr bool is_deserialize_defined_v =
    utils::is_detected_exact_v<result<void>,
                               details::tagged_deserialize_result_t, T>;

template <typename T>
inline constexpr bool is_bit_size_defined_v =
    utils::is_detected_exact_v<std::size_t, details::bit_size_result_t, T>;

template <typename T>
constexpr std::size_t bit_sizeof(const T &aValue) noexcept;

namespace details
{
template <typename T, bool IsAggregate>
struct is_simple_serializable;

template <typename T, bool IsAggregate>
struct is_simple_deserializable;

template <typename T, bool IsAggregate>
struct is_serializable;

template <typename T, bool IsAggregate>
struct is_deserializable;

template <typename T, bool IsAggregate, bool IsTaggedBitSizeDefined>
struct is_compile_time_computable_size;

template <typename T, bool IsAggregate, bool IsTaggedBitSizeDefined>
struct compile_time_bit_size;

template <typename T>
struct aggregate
{
    template <std::size_t... I>
    static std::bool_constant<
        (... && is_simple_serializable<
                    pfr::tuple_element_t<I, T>,
                    std::is_aggregate_v<pfr::tuple_element_t<I, T>>>::value)>
        isSimpleSerializable(std::index_sequence<I...>);

    template <std::size_t... I>
    static std::bool_constant<
        (... && is_simple_deserializable<
                    pfr::tuple_element_t<I, T>,
                    std::is_aggregate_v<pfr::tuple_element_t<I, T>>>::value)>
        isSimpleDeserializable(std::index_sequence<I...>);

    template <std::size_t... I>
    static std::bool_constant<
        (... && is_serializable<
                    pfr::tuple_element_t<I, T>,
                    std::is_aggregate_v<pfr::tuple_element_t<I, T>>>::value)>
        isSerializable(std::index_sequence<I...>);

    template <std::size_t... I>
    static std::bool_constant<
        (... && is_deserializable<
                    pfr::tuple_element_t<I, T>,
                    std::is_aggregate_v<pfr::tuple_element_t<I, T>>>::value)>
        isDeserializable(std::index_sequence<I...>);

    template <std::size_t... I>
    static std::bool_constant<
        (... &&
         is_compile_time_computable_size<
             pfr::tuple_element_t<I, T>,
             std::is_aggregate_v<pfr::tuple_element_t<I, T>>,
             is_tagged_bit_size_defined_v<pfr::tuple_element_t<I, T>>>::value)>
        isCompileComputableSize(std::index_sequence<I...>);

    template <std::size_t... I>
    static std::integral_constant<
        std::size_t,
        (... +
         compile_time_bit_size<
             pfr::tuple_element_t<I, T>,
             std::is_aggregate_v<pfr::tuple_element_t<I, T>>,
             is_tagged_bit_size_defined_v<pfr::tuple_element_t<I, T>>>::value)>
        compileTimeBitSizeOf(std::index_sequence<I...>);

    template <std::size_t... I>
    static std::size_t bitSizeOf(const T &aValue,
                                 std::index_sequence<I...>) noexcept
    {
        return (... + bit_sizeof(pfr::get<I>(aValue)));
    }
};

template <typename T, bool IsAggregate>
struct is_simple_serializable
    : decltype(aggregate<T>::isSimpleSerializable(
          std::make_index_sequence<pfr::tuple_size_v<T>>{}))
{
};

template <typename T>
struct is_simple_serializable<T, false>
    : std::bool_constant<is_simple_serialize_defined_v<T>>
{
};

template <typename T, bool IsAggregate>
struct is_simple_deserializable
    : decltype(aggregate<T>::isSimpleDeserializable(
          std::make_index_sequence<pfr::tuple_size_v<T>>{}))
{
};

template <typename T>
struct is_simple_deserializable<T, false>
    : std::bool_constant<is_simple_deserialize_defined_v<T>>
{
};

template <typename T, bool IsAggregate>
struct is_serializable
    : decltype(aggregate<T>::isSerializable(
          std::make_index_sequence<pfr::tuple_size_v<T>>{}))
{
};

template <typename T>
struct is_serializable<T, false> : std::bool_constant<is_serialize_defined_v<T>>
{
};

template <typename T, bool IsAggregate>
struct is_deserializable
    : decltype(aggregate<T>::isDeserializable(
          std::make_index_sequence<pfr::tuple_size_v<T>>{}))
{
};

template <typename T>
struct is_deserializable<T, false>
    : std::bool_constant<is_deserialize_defined_v<T>>
{
};

template <typename T, bool IsAggregate, bool IsTaggedBitSizeDefined>
struct is_compile_time_computable_size : std::false_type
{
};

template <typename T, bool IsAggregate>
struct is_compile_time_computable_size<T, IsAggregate, true> : std::true_type
{
};

template <typename T>
struct is_compile_time_computable_size<T, true, false>
    : decltype(aggregate<T>::isCompileComputableSize(
          std::make_index_sequence<pfr::tuple_size_v<T>>{}))
{
};

template <typename T, bool IsAggregate>
struct compile_time_bit_size<T, IsAggregate, true>
    : std::integral_constant<std::size_t, bit_size(tag<T>)>
{
};

template <typename T>
struct compile_time_bit_size<T, true, false>
    : decltype(aggregate<T>::compileTimeBitSizeOf(
          std::make_index_sequence<pfr::tuple_size_v<T>>{}))
{
};

template <typename T, bool IsAggregate, bool IsBitSizeDefined>
struct run_time_bit_size;

template <typename T>
struct run_time_bit_size<T, true, false>
{
    static std::size_t get(const T &aValue) noexcept
    {
        return aggregate<T>::bitSizeOf(
            aValue, std::make_index_sequence<pfr::tuple_size_v<T>>{});
    }
};

template <typename T, bool IsAggregate>
struct run_time_bit_size<T, IsAggregate, true>
{
    static std::size_t get(const T &aValue) noexcept
    {
        return SizeChecker<T>::bit_size(aValue);
    }
};
}  // namespace details

template <typename T>
inline constexpr bool is_simple_serializable_v =
    details::is_simple_serializable<T, std::is_aggregate_v<T>>::value;

template <typename T>
inline constexpr bool is_simple_deserializable_v =
    details::is_simple_deserializable<T, std::is_aggregate_v<T>>::value;

template <typename T>
inline constexpr bool is_serializable_v =
    details::is_serializable<T, std::is_aggregate_v<T>>::value;

template <typename T>
inline constexpr bool is_deserializable_v =
    details::is_deserializable<T, std::is_aggregate_v<T>>::value;

template <typename T>
inline constexpr bool is_compile_time_computable_size_v =
    details::is_compile_time_computable_size<
        T, std::is_aggregate_v<T>,
        details::is_tagged_bit_size_defined_v<T>>::value;

template <typename T>
inline constexpr std::size_t bit_sizeof_v = details::compile_time_bit_size<
    T, std::is_aggregate_v<T>, details::is_tagged_bit_size_defined_v<T>>::value;

template <typename T>
constexpr std::size_t bit_sizeof(const T &aValue) noexcept
{
    if constexpr (is_compile_time_computable_size_v<T>)
    {
        return bit_sizeof_v<T>;
    }
    else
    {
        return details::run_time_bit_size<
            T, std::is_aggregate_v<T>, is_bit_size_defined_v<T>>::get(aValue);
    }
}

template <typename T>
constexpr std::size_t byte_sizeof(const T &aValue) noexcept
{
    const auto kNBits = bit_sizeof(aValue);
    return kNBits / CHAR_BIT + (kNBits % CHAR_BIT ? 1 : 0);
}
}  // namespace rabbit

#endif /* rabbit_typedefs_h */
