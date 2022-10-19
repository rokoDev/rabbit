#ifndef rabbit_deserialize_h
#define rabbit_deserialize_h

#include <utils/utils.h>

#include <type_traits>
#include <utility>

#include "bin_reader.h"

namespace rabbit
{
namespace details
{
template <typename T, typename... Ts>
eReaderError deserializeAggregateImpl(simple_reader &aReader,
                                      Ts &...aArgs) noexcept
{
    static_assert(std::is_aggregate_v<T>, "T must be aggregate type.");
    eReaderError retVal{eReaderError::kSuccess};
    [[maybe_unused]] const bool isSuccess =
        (... &&
         (eReaderError::kSuccess == (retVal = deserialize(aReader, aArgs))));
    return retVal;
}

template <typename T, std::size_t... I>
eReaderError deserializeAggregate(simple_reader &aReader, T &aValue,
                                  std::index_sequence<I...>) noexcept
{
    static_assert(std::is_aggregate_v<T>, "T must be aggregate type.");
    return deserializeAggregateImpl<T>(aReader, pfr::get<I>(aValue)...);
}

template <typename T, typename... Ts>
result<void> deserializeAggregateImpl(reader &aReader, Ts &...aArgs) noexcept
{
    static_assert(std::is_aggregate_v<T>, "T must be aggregate type.");
    if ((... && deserialize<Ts>(aReader, aArgs)))
    {
        return {};
    }
    else
    {
        return leaf::current_error();
    }
}

template <typename T, std::size_t... I>
result<void> deserializeAggregate(reader &aReader, T &aValue,
                                  std::index_sequence<I...>) noexcept
{
    static_assert(std::is_aggregate_v<T>, "T must be aggregate type.");
    return deserializeAggregateImpl<T>(aReader, pfr::get<I>(aValue)...);
}
}  // namespace details

template <typename T>
eReaderError deserialize(simple_reader &aReader, T &aValue) noexcept
{
    static_assert(is_simple_deserializable_v<T>, "T is not serializable type.");
    if constexpr (is_simple_deserialize_defined_v<T>)
    {
        return deserialize<T>(aReader, aValue, tag<T>);
    }
    else
    {
        using Indices = std::make_index_sequence<pfr::tuple_size_v<T>>;
        return details::deserializeAggregate<T>(aReader, aValue, Indices{});
    }
}

template <typename T>
result<void> deserialize(reader &aReader, T &aValue) noexcept
{
    static_assert(is_deserializable_v<T>, "T is not serializable type.");
    if constexpr (is_deserialize_defined_v<T>)
    {
        return deserialize<T>(aReader, aValue, tag<T>);
    }
    else
    {
        using Indices = std::make_index_sequence<pfr::tuple_size_v<T>>;
        return details::deserializeAggregate<T>(aReader, aValue, Indices{});
    }
}

template <typename T>
result<T> deserialize(reader &aReader) noexcept
{
    T retVal{};
    BOOST_LEAF_CHECK(deserialize(aReader, retVal));
    return retVal;
}
}  // namespace rabbit

#endif /* rabbit_deserialize_h */
