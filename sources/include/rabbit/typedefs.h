#ifndef rabbit_typedefs_h
#define rabbit_typedefs_h

#include <buffer/buffer.h>
#include <strong_type/strong_type.h>

#include <boost/leaf.hpp>
#include <boost/pfr.hpp>

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

using SrcBitOffset =
    strong::strong_type<struct SrcBitOffsetTag, uint_fast8_t, NecessaryOps>;
using DstBitOffset =
    strong::strong_type<struct DstBitOffsetTag, uint_fast8_t, NecessaryOps>;
using BitOffset =
    strong::strong_type<struct BitOffsetTag, uint_fast8_t, NecessaryOps>;
using NumBits = buffer::n_bits;

template <typename StrongT>
struct DataPtrOps
    : strong::indirection<StrongT>
    , strong::subscription<StrongT>
    , strong::comparisons<StrongT>
    , strong::implicitly_convertible_to_underlying<StrongT>
{
};

using Src = strong::strong_type<struct SrcTag, uint8_t const *, DataPtrOps>;
using Dst = strong::strong_type<struct DstTag, uint8_t *, DataPtrOps>;

using buf_view = buffer::buffer_view<uint8_t>;
using buf_view_const = buffer::buffer_view_const<buf_view::value_type>;
using bit_pos = buffer::bit_pos;
using n_bytes = buffer::n_bytes;

class Core;

template <typename ImplT>
class bin_writer;

using writer = bin_writer<Core>;

template <typename ImplT>
class bin_reader;

using reader = bin_reader<Core>;

template <typename T>
result<void> serialize(writer &aWriter, T &&aValue) noexcept;

template <typename T>
result<T> deserialize(reader &aReader) noexcept;

namespace details
{
template <class T>
using tagged_serialize_result_t =
    decltype(serialize(std::declval<writer &>(), std::declval<T>(), tag<T>));

template <class T>
using serialize_result_t =
    decltype(serialize(std::declval<writer &>(), std::declval<T>()));

template <class T>
using tagged_deserialize_result_t =
    decltype(deserialize(std::declval<reader &>(), tag<T>));

template <class T>
using deserialize_result_t = decltype(deserialize<T>(std::declval<reader &>()));
}  // namespace details

template <typename T>
inline constexpr bool is_serialize_defined_v =
    utils::is_detected_exact_v<result<void>, details::tagged_serialize_result_t,
                               T>;

template <typename T>
inline constexpr bool is_deserialize_defined_v =
    utils::is_detected_exact_v<result<T>, details::tagged_deserialize_result_t,
                               T>;

namespace details
{
template <typename T, bool IsAggregate>
struct is_serializable;

template <typename T, bool IsAggregate>
struct is_deserializable;

template <typename T>
struct aggregate
{
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
};

template <typename T, bool IsAggregate>
struct is_serializable : std::bool_constant<is_serialize_defined_v<T>>
{
};

template <typename T>
struct is_serializable<T, true>
    : decltype(aggregate<T>::isSerializable(
          std::make_index_sequence<pfr::tuple_size_v<T>>{}))
{
};

template <typename T, bool IsAggregate>
struct is_deserializable : std::bool_constant<is_deserialize_defined_v<T>>
{
};

template <typename T>
struct is_deserializable<T, true>
    : decltype(aggregate<T>::isDeserializable(
          std::make_index_sequence<pfr::tuple_size_v<T>>{}))
{
};
}  // namespace details

template <typename T>
inline constexpr bool is_serializable_v =
    details::is_serializable<T, std::is_aggregate_v<T>>::value;

template <typename T>
inline constexpr bool is_deserializable_v =
    details::is_deserializable<T, std::is_aggregate_v<T>>::value;
}  // namespace rabbit

#endif /* rabbit_typedefs_h */
