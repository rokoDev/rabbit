#ifndef rabbit_deserialize_h
#define rabbit_deserialize_h

#include <utils/utils.h>

#include <type_traits>
#include <utility>

#include "bin_reader.h"

namespace rabbit
{
template <typename T>
result<T> deserialize(reader &aReader) noexcept;

namespace details
{
template <class T>
using tagged_deserialize_result_t =
    decltype(deserialize(std::declval<reader &>(), tag<T>));

template <class T>
using deserialize_result_t = decltype(deserialize<T>(std::declval<reader &>()));
}  // namespace details

template <typename T>
inline constexpr bool is_deserialize_defined_v =
    utils::is_detected_exact_v<result<T>, details::tagged_deserialize_result_t,
                               T>;

namespace details
{
template <typename T>
struct is_deserializable_t;

template <typename T, std::size_t... I>
std::enable_if_t<(... &&
                  is_deserializable_t<pfr::tuple_element_t<I, T>>::value),
                 std::true_type>
    is_deserializable_aggregate(std::index_sequence<I...>);

template <typename T, std::size_t... I>
std::false_type is_deserializable_aggregate(...);

template <typename T>
using is_deserializable_aggregate_t = decltype(is_deserializable_aggregate<T>(
    std::make_index_sequence<pfr::tuple_size_v<T>>{}));

template <typename T>
struct is_deserializable_t
    : std::conditional_t<
          is_deserialize_defined_v<T>, std::true_type,
          std::conditional_t<std::is_aggregate_v<T>,
                             is_deserializable_aggregate_t<T>, std::false_type>>
{
};
}  // namespace details

template <typename T>
inline constexpr bool is_deserializable_v =
    details::is_deserializable_t<T>::value;

namespace details
{
template <typename T, typename... Ts>
result<T> deserializeAggregateImpl(reader &aReader,
                                   result<Ts>... aArgs) noexcept
{
    static_assert(std::is_aggregate_v<T>, "T must be aggregate type.");
    if ((... && (aArgs = deserialize<Ts>(aReader))))
    {
        return T{(std::move(*aArgs))...};
    }
    else
    {
        return leaf::current_error();
    }
}

template <typename T, std::size_t... I>
result<T> deserializeAggregate(reader &aReader,
                               std::index_sequence<I...>) noexcept
{
    static_assert(std::is_aggregate_v<T>, "T must be aggregate type.");
    return deserializeAggregateImpl<T>(aReader,
                                       result<pfr::tuple_element_t<I, T>>{}...);
}
}  // namespace details

template <typename T>
result<T> deserialize(reader &aReader) noexcept
{
    static_assert(is_deserializable_v<T>, "T is not serializable type.");
    if constexpr (is_deserialize_defined_v<T>)
    {
        return deserialize<T>(aReader, tag<T>);
    }
    else
    {
        using Indices = std::make_index_sequence<pfr::tuple_size_v<T>>;
        return details::deserializeAggregate<T>(aReader, Indices{});
    }
}
}  // namespace rabbit

#endif /* rabbit_deserialize_h */
