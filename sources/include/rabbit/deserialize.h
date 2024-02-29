#ifndef rabbit_deserialize_h
#define rabbit_deserialize_h

#include <utils/utils.h>

#include <type_traits>
#include <utility>

#include "bin_reader.h"

namespace rabbit
{
CREATE_MEMBER_TYPE_CHECKERS(size_type)
CREATE_MEMBER_TYPE_CHECKERS(const_iterator)
CREATE_R_METHOD_CHECKERS(reserve)
CREATE_R_METHOD_CHECKERS(resize)
CREATE_R_METHOD_CHECKERS(push_back)
CREATE_METHOD_CHECKERS(insert)
namespace details
{
template <typename T, typename Reader, bool CheckSize>
constexpr decltype(auto) deserialize_impl(Reader &aReader, T &aValue) noexcept;

template <typename Reader, typename SizeType>
decltype(auto) deserialize_std_size(Reader &aReader, SizeType &aValue) noexcept
{
    const auto num_steps_minus_1 = aReader.template getValue<std::uint8_t>(
        NumBits{default_size_traits::descr_bit_size});
    const auto num_bits_to_take =
        num_steps_minus_1 * default_size_traits::bits_per_step;
    using result_adapter = typename Reader::result_adapter_t;
    if (aReader.bits_taken() + num_bits_to_take > aReader.bits_size())
    {  // not taken bits are not enough to store size of elements in container
        return result_adapter::template new_error<void>(
            reader_error::run_out_of_data_source);
    }
    const std::size_t num_steps = num_steps_minus_1 + 1;
    if (num_steps * default_size_traits::bits_per_step >
        utils::num_bits<SizeType>())
    {  // stored count of elements are not representable via std::size_t
        return result_adapter::template new_error<void>(
            reader_error::value_is_not_representable_via_std_size_t);
    }
    aReader.take_bits(num_bits_to_take);
    aValue = aReader.template getValue<SizeType>(
        NumBits{num_bits_to_take + default_size_traits::bits_per_step});
    return result_adapter::template success();
}

template <typename Reader, typename Container, typename DeserializeElements>
decltype(auto) deserialize_container(
    Reader &aReader, Container &aContainer,
    DeserializeElements &aDeserializeElements) noexcept
{
    //  TODO: forbid to call this function for std::array and bounded C-arrays:
    //  T (&)[N]

    //    static_assert(has_type_size_type_v<Container>);
    using size_type = typename Container::size_type;

    size_type num_elements = 0;
    if (auto r = deserialize_std_size(aReader, num_elements); !r)
    {
        return r;
    }

    if (auto r = take_minimum_bits_for_container_elements<
            Container, reader_error::run_out_of_data_source>(aReader,
                                                             num_elements);
        !r)
    {
        return r;
    }

    return aDeserializeElements(aReader, aContainer, num_elements);
}

template <typename ValueType, typename Reader, typename Container,
          typename SizeType, typename Add>
decltype(auto) deserialize_each_element_to_container(Reader &aReader,
                                                     Container &aContainer,
                                                     SizeType aSize,
                                                     Add &&aAddOp) noexcept
{
    using result_adapter = typename Reader::result_adapter_t;
    using value_type = ValueType;
    for (SizeType i = 0; i < aSize; ++i)
    {
        value_type element;
        auto r = deserialize_impl<value_type, Reader, false>(aReader, element);
        if (r != result_adapter::template success())
        {
            return r;
        }
        aAddOp(aContainer, std::move(element));
    }
    return result_adapter::template success();
}

template <typename Reader, typename T>
decltype(auto) deserialize(
    Reader &aReader, T &aContainer,
    tag_t<std::enable_if_t<
        std::disjunction_v<is_std_vector<T>, is_std_basic_string<T>>>>)
{
    using value_type = typename T::value_type;
    using size_type = typename T::size_type;
    auto deserialize_elements_to_container =
        [](Reader &argReader, T &argContainer, size_type aSize)
    {
        if constexpr (
            is_size_defined_by_type_v<value_type, Reader::template tag_t> &&
            (std::alignment_of_v<value_type> ==
             std::alignment_of_v<
                 std::byte>)&&(size_defined_by_type_v<value_type,
                                                      Reader::template tag_t> ==
                               utils::num_bits<std::byte>()))
        {
            // TODO: add error handling
            argContainer.resize(aSize);

            argReader.getBits(
                Dst{reinterpret_cast<std::byte *>(argContainer.data())},
                DstOffset{0}, NumBits{aSize * CHAR_BIT});

            using result_adapter = typename Reader::result_adapter_t;
            return result_adapter::template success();
        }
        else
        {
            // TODO: add error handling
            argContainer.reserve(aSize);

            return deserialize_each_element_to_container<value_type>(
                argReader, aSize,
                [](T &aContainerRef, auto &&aElement)
                {
                    // TODO: add error handling
                    aContainerRef.push_back(
                        std::forward<decltype(aElement)>(aElement));
                });
        }
    };
    return deserialize_container(aReader, aContainer,
                                 std::move(deserialize_elements_to_container));
}

template <typename Reader, typename T>
decltype(auto) deserialize(Reader &aReader, T &aContainer,
                           tag_t<std::enable_if_t<is_std_valarray_v<T>>>)
{
    using value_type = typename T::value_type;
    using size_type = typename T::size_type;
    auto deserialize_elements_to_container =
        [](Reader &argReader, T &argContainer, size_type aSize)
    {
        // TODO: add error handling
        argContainer.resize(aSize);

        using result_adapter = typename Reader::result_adapter_t;

        for (auto &element: argContainer)
        {
            auto r =
                deserialize_impl<value_type, Reader, false>(argReader, element);
            if (r != result_adapter::template success())
            {
                return r;
            }
        }
        return result_adapter::template success();
    };
    return deserialize_container(aReader, aContainer,
                                 std::move(deserialize_elements_to_container));
}

template <typename Reader, typename T>
decltype(auto) deserialize(
    Reader &aReader, T &aContainer,
    tag_t<std::enable_if_t<std::disjunction_v<
        is_std_set<T>, is_std_map<T>, is_std_multiset<T>, is_std_multimap<T>,
        is_std_unordered_set<T>, is_std_unordered_multiset<T>,
        is_std_unordered_map<T>, is_std_unordered_multimap<T>, is_std_list<T>,
        is_std_deque<T>, is_std_queue<T>>>>)
{
    using value_type = typename T::value_type;
    using size_type = typename T::size_type;
    auto deserialize_elements_to_container =
        [](Reader &argReader, T &, size_type aSize)
    {
        return deserialize_each_element_to_container<value_type>(
            argReader, aSize,
            [](T &aContainerRef, auto &&aElement)
            {
                // TODO: add error handling
                aContainerRef.insert(
                    std::forward<decltype(aElement)>(aElement));
            });
    };
    return deserialize_container(aReader, aContainer,
                                 std::move(deserialize_elements_to_container));
}

template <typename T, typename Reader, typename... Ts>
decltype(auto) deserialize_aggregate_impl(Reader &aReader,
                                          Ts &...aArgs) noexcept
{
    static_assert(std::is_aggregate_v<T>, "T must be aggregate type.");
    using R = typename Reader::result_adapter_t::template result_t<void>;
    R r;
    [[maybe_unused]] const bool success =
        (... && (r = deserialize_impl<Ts, Reader, false>(aReader, aArgs)));
    return std::move(r);
}

template <typename T, typename Reader, std::size_t... I>
decltype(auto) deserialize_aggregate(Reader &aReader, T &aValue,
                                     std::index_sequence<I...>) noexcept
{
    static_assert(std::is_aggregate_v<T>, "T must be aggregate type.");
    return deserialize_aggregate_impl<T>(aReader, pfr::get<I>(aValue)...);
}

template <typename T, typename Reader, bool CheckSize>
constexpr decltype(auto) deserialize_impl(Reader &aReader, T &aValue) noexcept
{
    if constexpr (CheckSize)
    {
        using result_adapter = typename Reader::result_adapter_t;
        constexpr auto size_by_type = Reader::template size_by_type<T>;
        if constexpr (!size_by_type)
        {
            return result_adapter::template success();
        }
        static_assert(size_by_type > 0,
                      "undefined function: constexpr std::size_t "
                      "rabbit_bit_size(tag_t<T>)");
        if (aReader.bits_taken() + size_by_type > aReader.bits_size())
        {
            return result_adapter::template new_error(
                reader_error::run_out_of_data_source);
        }
        aReader.take_bits(size_by_type);
    }
    static_assert(is_deserializable_v<T, Reader>,
                  "T is not deserializable type.");
    if constexpr (is_deserialize_defined_v<T, Reader>)
    {
        using tag_type = typename Reader::template tag_t<T>;
        return deserialize(aReader, aValue, tag_type{});
    }
    else
    {
        using Indices = std::make_index_sequence<pfr::tuple_size_v<T>>;
        return deserialize_aggregate(aReader, aValue, Indices{});
    }
}
}  // namespace details

template <typename T, typename Reader>
constexpr decltype(auto) deserialize(Reader &aReader, T &aValue) noexcept
{
    return details::deserialize_impl<T, Reader, true>(aReader, aValue);
}
}  // namespace rabbit

#endif /* rabbit_deserialize_h */
