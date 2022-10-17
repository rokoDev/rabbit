#ifndef rabbit_serialize_h
#define rabbit_serialize_h

#include <utils/utils.h>

#include <type_traits>
#include <utility>

#include "bin_writer.h"

namespace rabbit
{
namespace details
{
template <typename T, std::size_t... I>
void serializeAggregate(simple_writer &aWriter, T &&aValue,
                        std::index_sequence<I...>) noexcept
{
    static_assert(std::is_aggregate_v<utils::remove_cvref_t<T>>,
                  "T must be aggregate type.");
    (..., serialize(aWriter, pfr::get<I>(std::forward<T>(aValue))));
}

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
void serialize(simple_writer &aWriter, T &&aValue) noexcept
{
    using CoreT = utils::remove_cvref_t<T>;
    static_assert(is_simple_serializable_v<CoreT>,
                  "T is not serializable type.");
    if constexpr (is_simple_serialize_defined_v<T>)
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
