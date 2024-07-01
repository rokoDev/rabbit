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
template <typename T, typename Writer, bool CheckSize>
constexpr decltype(auto) serialize_impl(Writer &aWriter, T &&aValue) noexcept;

template <typename Writer, typename SizeType>
decltype(auto) serialize_std_size(Writer &aWriter, SizeType aValue) noexcept
{
    using size_type_traits = default_size_traits;
    static_assert(sizeof(SizeType) <= sizeof(size_type_traits::value_type));
    const auto step_count = size_type_traits::step_count(aValue);
    const auto num_bits_for_value =
        step_count * size_type_traits::bits_per_step;
    const auto num_bits_to_take =
        size_type_traits::descr_bit_size + num_bits_for_value;

    if (aWriter.bits_taken() + num_bits_to_take > aWriter.bits_size())
    {  // not taken bits are not enough to store size of elements in container
        return Writer::new_error(writer_error::not_enough_buffer_size);
    }

    aWriter.take_bits(num_bits_to_take);
    aWriter.addValue(step_count - 1, NumBits{size_type_traits::descr_bit_size});
    aWriter.addValue(aValue, NumBits{num_bits_for_value});
    return Writer::success();
}

template <typename Writer, typename Container, typename SerializeElements>
decltype(auto) serialize_container(
    Writer &aWriter, Container &aContainer,
    SerializeElements &aSerializeElements) noexcept
{
    //  TODO: forbid to call this function for std::array and bounded C-arrays:
    //  T (&)[N]

    //    static_assert(has_type_size_type_v<Container>);
    using size_type = typename Container::size_type;

    size_type num_elements = aContainer.size();
    if (auto r = serialize_std_size(aWriter, num_elements); !r)
    {
        return r;
    }

    if (auto r = take_minimum_bits_for_container_elements<
            Container, writer_error::not_enough_buffer_size>(aWriter,
                                                             num_elements);
        !r)
    {
        return r;
    }

    return aSerializeElements(aWriter, aContainer, num_elements);
}

template <typename Writer, typename Container>
decltype(auto) serialize_container_ordinary(Writer &aWriter,
                                            Container &aContainer) noexcept
{
    using value_type = typename Container::value_type;
    auto success{Writer::success()};
    for (const auto &element: aContainer)
    {
        auto r = serialize_impl<value_type, Writer, false>(aWriter, element);
        if (r != success)
        {
            return r;
        }
    }
    return std::move(success);
}

template <typename Writer, typename T>
decltype(auto) serialize(
    Writer &aWriter, T &aContainer,
    tag_t<std::enable_if_t<
        std::disjunction_v<is_std_vector<T>, is_std_basic_string<T>>>>)
{
    using value_type = typename T::value_type;

    if constexpr (
        is_size_defined_by_type_v<value_type, Writer::template tag_t> &&
        (std::alignment_of_v<value_type> == std::alignment_of_v<std::byte>)&&(
            size_defined_by_type_v<value_type, Writer::template tag_t> ==
            utils::num_bits<std::byte>()))
    {
        auto serialize_elements_to_container =
            [](Writer &argWriter, T &argContainer)
        {
            argWriter.addBits(
                Src{reinterpret_cast<std::byte *>(argContainer.data())},
                NumBits{argContainer.size() * CHAR_BIT});

            return Writer::success();
        };
        return serialize_container(aWriter, aContainer,
                                   std::move(serialize_elements_to_container));
    }
    else
    {
        return serialize_container(aWriter, aContainer,
                                   serialize_container_ordinary<Writer, T>);
    }
}

template <typename Writer, typename T>
decltype(auto) serialize(
    Writer &aWriter, T &aContainer,
    tag_t<std::enable_if_t<std::disjunction_v<
        is_std_set<T>, is_std_map<T>, is_std_multiset<T>, is_std_multimap<T>,
        is_std_unordered_set<T>, is_std_unordered_multiset<T>,
        is_std_unordered_map<T>, is_std_unordered_multimap<T>, is_std_list<T>,
        is_std_deque<T>, is_std_queue<T>, is_std_valarray<T>>>>)
{
    return serialize_container(aWriter, aContainer,
                               serialize_container_ordinary<Writer, T>);
}

template <typename T, typename Writer, typename... Ts>
decltype(auto) serialize_aggregate_impl(Writer &aWriter, Ts &...aArgs) noexcept
{
    static_assert(std::is_aggregate_v<T>);
    using R = typename Writer::base::result;
    R r{};
    [[maybe_unused]] const bool success =
        (... && (r = std::move(serialize_impl<decltype(aArgs), Writer, false>(
                     aWriter, aArgs)),
                 Writer::is_success(r)));
    return r;
}

template <typename T, typename Writer, std::size_t... I>
decltype(auto) serialize_aggregate(Writer &aWriter, T &&aValue,
                                   std::index_sequence<I...>) noexcept
{
    using V = utils::remove_cvref_t<T>;
    static_assert(std::is_aggregate_v<V>);
    return serialize_aggregate_impl<V>(aWriter, pfr::get<I>(aValue)...);
}

template <typename T, typename Writer, bool CheckSize>
constexpr decltype(auto) serialize_impl(Writer &aWriter, T &&aValue) noexcept
{
    using V = utils::remove_cvref_t<T>;
    if constexpr (CheckSize)
    {
        constexpr auto size_by_type = Writer::template size_by_type<V>;
        if constexpr (!size_by_type)
        {
            return Writer::success();
        }
        //        static_assert(size_by_type > 0, "undefined function: constexpr
        //        std::size_t rabbit_bit_size(tag_t<T>)");
        if (aWriter.bits_taken() + size_by_type > aWriter.bits_size())
        {
            return Writer::new_error(writer_error::not_enough_buffer_size);
        }
        aWriter.take_bits(size_by_type);
    }
    static_assert(is_serializable_v<V, Writer>);
    if constexpr (is_serialize_defined_v<V, Writer>)
    {
        using tag_type = typename Writer::template tag_t<V>;
        return serialize(aWriter, std::forward<T>(aValue), tag_type{});
    }
    else
    {
        using Indices = std::make_index_sequence<pfr::tuple_size_v<V>>;
        return serialize_aggregate(aWriter, std::forward<T>(aValue), Indices{});
    }
}
}  // namespace details

template <typename T, typename Writer>
constexpr decltype(auto) serialize(Writer &aWriter, T &&aValue) noexcept
{
    return details::serialize_impl<T, Writer, true>(aWriter,
                                                    std::forward<T>(aValue));
}
}  // namespace rabbit

#endif /* rabbit_serialize_h */
