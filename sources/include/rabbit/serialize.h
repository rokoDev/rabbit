#ifndef rabbit_serialize_h
#define rabbit_serialize_h

#include <utils/utils.h>

#include <type_traits>
#include <utility>

#include "bin_writer.h"

namespace rabbit
{
template <typename T>
result<void> serialize(writer &aWriter, T &&aValue) noexcept;

namespace details
{
template <class T>
using tagged_serialize_result_t =
    decltype(serialize(std::declval<writer &>(), std::declval<T>(), tag<T>));

template <class T>
using serialize_result_t =
    decltype(serialize(std::declval<writer &>(), std::declval<T>()));
}  // namespace details

template <typename T>
inline constexpr bool is_serialize_defined_v =
    utils::is_detected_exact_v<result<void>, details::tagged_serialize_result_t,
                               T>;

namespace details
{
template <typename T>
struct is_serializable_t;

template <typename T, std::size_t... I>
std::enable_if_t<(... && is_serializable_t<pfr::tuple_element_t<I, T>>::value),
                 std::true_type>
    is_serializable_aggregate(std::index_sequence<I...>);

template <typename T, std::size_t... I>
std::false_type is_serializable_aggregate(...);

template <typename T>
using is_serializable_aggregate_t = decltype(is_serializable_aggregate<T>(
    std::make_index_sequence<pfr::tuple_size_v<T>>{}));

template <typename T>
struct is_serializable_t
    : std::conditional_t<
          is_serialize_defined_v<T>, std::true_type,
          std::conditional_t<std::is_aggregate_v<T>,
                             is_serializable_aggregate_t<T>, std::false_type>>
{
};
}  // namespace details

template <typename T>
inline constexpr bool is_serializable_v = details::is_serializable_t<T>::value;

namespace details
{
template <typename T, std::size_t... I>
result<void> serializeAggregate(writer &aWriter, T &&aValue,
                                std::index_sequence<I...>) noexcept
{
    static_assert(std::is_aggregate_v<utils::remove_cvref_t<T>>,
                  "T must be aggregate type.");
    if ((... && serialize(aWriter, pfr::get<I>(std::forward<T>(aValue)))))
    {
        return {};
    }
    else
    {
        return leaf::current_error();
    }
}
}  // namespace details

template <typename T>
result<void> serialize(writer &aWriter, T &&aValue) noexcept
{
    using CoreT = utils::remove_cvref_t<T>;
    static_assert(is_serializable_v<CoreT>, "T is not serializable type.");
    if constexpr (is_serialize_defined_v<T>)
    {
        return serialize(aWriter, std::forward<T>(aValue), tag<T>);
    }
    else
    {
        using Indices = std::make_index_sequence<pfr::tuple_size_v<CoreT>>;
        return details::serializeAggregate<T>(aWriter, std::forward<T>(aValue),
                                              Indices{});
    }
}
}  // namespace rabbit

#endif /* rabbit_serialize_h */
