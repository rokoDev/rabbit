#ifndef rabbit_typedefs_h
#define rabbit_typedefs_h

#include <buffer/buffer.h>
#include <interval/interval.h>
#include <strong_type/strong_type.h>
#include <utils/utils.h>

#include <boost/pfr.hpp>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <forward_list>
#include <list>
#include <map>
#include <queue>
#include <set>
#include <string>
#include <string_view>
#include <type_traits>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <valarray>
#include <vector>

namespace rabbit
{
namespace pfr = boost::pfr;

template <typename T>
struct tag_t
{
};

template <typename T>
inline constexpr tag_t<T> tag{};

enum class reader_error
{
    success = 0,
    invalid_start_pos,
    not_enough_buffer_size,
    num_bits_exceed_type_size,
    null_dst_bits_array,
    invalid_destination,
    dst_offset_too_big,
    read_more_than_destination_size,
    invalid_interval_index,
    non_empty_vector_size_is_zero,
    run_out_of_data_source,
    value_is_not_representable_via_std_size_t
};

enum class writer_error
{
    success = 0,
    invalid_start_pos,
    not_enough_buffer_size,
    num_bits_exceed_type_size,
    null_src_bits_array,
    write_more_than_source_size,
    value_below_interval,
    value_above_interval,
    vector_size_is_too_big,
};

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

using buffer_view = buffer::buffer_view<std::byte>;
using buffer_view_const = buffer::buffer_view_const<buffer_view::value_type>;
using bit_pos = buffer::bit_pos;
using n_bytes = buffer::n_bytes;

template <typename Derived, typename Result>
struct result_adapter_base
{
    using derived = Derived;

    result_adapter_base() = delete;
    result_adapter_base(const result_adapter_base &) = delete;
    result_adapter_base &operator=(const result_adapter_base &) = delete;
    result_adapter_base(result_adapter_base &&) = delete;
    result_adapter_base &operator=(result_adapter_base &&) = delete;
    ~result_adapter_base() = delete;

    template <typename... Args>
    static constexpr decltype(auto) new_error(Args &&...aArgs) noexcept
    {
        return derived::new_error(std::forward<Args>(aArgs)...);
    }

    static constexpr decltype(auto) success() noexcept
    {
        return derived::success();
    }

    template <typename R>
    static constexpr bool is_success(R &&aResult) noexcept
    {
        static_assert(std::is_same_v<utils::remove_cvref_t<R>, Result>);
        return derived::is_success(std::forward<R>(aResult));
    }

    template <typename R>
    static constexpr bool is_error(R &&aResult) noexcept
    {
        return derived::is_error(std::forward<R>(aResult));
    }
};

template <typename E,
          typename = std::enable_if_t<std::disjunction_v<
              std::is_same<E, reader_error>, std::is_same<E, writer_error>>>>
struct enum_result_adapter
    : public result_adapter_base<enum_result_adapter<E>, E>
{
   public:
    using result = E;
    using base = result_adapter_base<enum_result_adapter<result>, result>;
    friend base;

   private:
    template <typename... Args>
    static constexpr decltype(auto) new_error(Args &&...aArgs) noexcept
    {
        return result{std::forward<Args>(aArgs)...};
    }

    static constexpr decltype(auto) success() noexcept
    {
        return result::success;
    }

    template <typename R>
    static constexpr bool is_success(R &&aResult) noexcept
    {
        return aResult == result::success;
    }

    template <typename R>
    static constexpr bool is_error(R &&aResult) noexcept
    {
        return !is_success(std::forward<R>(aResult));
    }
};

using reader_error_result_adapter = enum_result_adapter<reader_error>;
using writer_error_result_adapter = enum_result_adapter<writer_error>;

template <typename CoreT, template <typename> class Tag, typename ResultAdapter,
          typename BufView = buffer_view>
class writer;

template <typename CoreT, template <typename> class Tag, typename ResultAdapter,
          typename BufView = buffer_view_const>
class reader;

template <typename T, typename Writer>
constexpr decltype(auto) serialize(Writer &aWriter, T &&aValue) noexcept;

template <typename T, typename Reader>
constexpr decltype(auto) deserialize(Reader &aReader, T &aValue) noexcept;

namespace details
{
CREATE_R_METHOD_CHECKERS(size)

CREATE_MEMBER_TYPE_CHECKERS(iterator)
CREATE_METHOD_CHECKERS(begin)
CREATE_METHOD_CHECKERS(end)

CREATE_MEMBER_TYPE_CHECKERS(const_iterator)
CREATE_METHOD_CHECKERS(cbegin)
CREATE_METHOD_CHECKERS(cend)

CREATE_FREE_FUNCTION_CHECKERS(begin)
CREATE_FREE_FUNCTION_CHECKERS(end)

CREATE_FREE_FUNCTION_CHECKERS(cbegin)
CREATE_FREE_FUNCTION_CHECKERS(cend)

template <typename T>
struct is_iterable
    : std::disjunction<
          std::conjunction<has_type_iterator<T>, has_invocable_begin<T>,
                           has_invocable_end<T>>,
          std::conjunction<is_begin_invocable<T>, is_end_invocable<T>>>
{
};

template <typename T>
inline constexpr bool is_iterable_v = is_iterable<T>::value;

template <typename T>
struct is_const_iterable
    : std::disjunction<
          std::conjunction<has_type_const_iterator<T>, has_invocable_cbegin<T>,
                           has_invocable_cend<T>>,
          std::conjunction<is_cbegin_invocable<T>, is_cend_invocable<T>>>
{
};

template <typename T>
inline constexpr bool is_const_iterable_v = is_const_iterable<T>::value;

template <typename T>
struct has_size_and_iterable_via_begin_end
    : std::conjunction<
          std::disjunction<has_invocable_r_size_const_noexcept<std::size_t, T>,
                           has_invocable_r_size_const<std::size_t, T>>,
          is_iterable<T>>
{
};

template <typename T>
inline constexpr bool has_size_and_iterable_via_begin_end_v =
    has_size_and_iterable_via_begin_end<T>::value;

template <typename T>
struct is_std_array : std::false_type
{
};

template <class T, std::size_t N>
struct is_std_array<std::array<T, N>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_array_v = is_std_array<T>::value;

template <typename T>
struct is_std_map : std::false_type
{
};

template <class Key, class T, class Compare, class Allocator>
struct is_std_map<std::map<Key, T, Compare, Allocator>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_map_v = is_std_map<T>::value;

template <typename T>
struct is_std_multimap : std::false_type
{
};

template <class Key, class T, class Compare, class Allocator>
struct is_std_multimap<std::multimap<Key, T, Compare, Allocator>>
    : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_multimap_v = is_std_multimap<T>::value;

template <typename T>
struct is_std_unordered_map : std::false_type
{
};

template <class Key, class T, class Hash, class KeyEqual, class Allocator>
struct is_std_unordered_map<
    std::unordered_map<Key, T, Hash, KeyEqual, Allocator>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_unordered_map_v = is_std_unordered_map<T>::value;

template <typename T>
struct is_std_unordered_multimap : std::false_type
{
};

template <class Key, class T, class Hash, class KeyEqual, class Allocator>
struct is_std_unordered_multimap<
    std::unordered_multimap<Key, T, Hash, KeyEqual, Allocator>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_unordered_multimap_v =
    is_std_unordered_multimap<T>::value;

template <typename T>
struct is_std_vector : std::false_type
{
};

template <typename T, typename Allocator>
struct is_std_vector<std::vector<T, Allocator>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_vector_v = is_std_vector<T>::value;

template <typename T>
struct is_std_set : std::false_type
{
};

template <typename Key, typename Compare, typename Allocator>
struct is_std_set<std::set<Key, Compare, Allocator>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_set_v = is_std_set<T>::value;

template <typename T>
struct is_std_multiset : std::false_type
{
};

template <typename Key, typename Compare, typename Allocator>
struct is_std_multiset<std::multiset<Key, Compare, Allocator>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_multiset_v = is_std_multiset<T>::value;

template <typename T>
struct is_std_unordered_set : std::false_type
{
};

template <typename Key, typename Hash, typename KeyEqual, typename Allocator>
struct is_std_unordered_set<std::unordered_set<Key, Hash, KeyEqual, Allocator>>
    : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_unordered_set_v = is_std_unordered_set<T>::value;

template <typename T>
struct is_std_unordered_multiset : std::false_type
{
};

template <typename Key, typename Hash, typename KeyEqual, typename Allocator>
struct is_std_unordered_multiset<
    std::unordered_multiset<Key, Hash, KeyEqual, Allocator>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_unordered_multiset_v =
    is_std_unordered_multiset<T>::value;

template <typename T>
struct is_std_list : std::false_type
{
};

template <typename T, typename Allocator>
struct is_std_list<std::list<T, Allocator>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_list_v = is_std_list<T>::value;

template <typename T>
struct is_std_basic_string : std::false_type
{
};

template <typename CharT, typename Traits, typename Allocator>
struct is_std_basic_string<std::basic_string<CharT, Traits, Allocator>>
    : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_basic_string_v = is_std_basic_string<T>::value;

template <typename T>
struct is_std_valarray : std::false_type
{
};

template <typename T>
struct is_std_valarray<std::valarray<T>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_valarray_v = is_std_valarray<T>::value;

template <typename T>
struct is_std_basic_string_view : std::false_type
{
};

template <typename CharT, typename Traits>
struct is_std_basic_string_view<std::basic_string_view<CharT, Traits>>
    : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_basic_string_view_v =
    is_std_basic_string_view<T>::value;

template <typename T>
struct is_std_forward_list : std::false_type
{
};

template <typename T, typename Allocator>
struct is_std_forward_list<std::forward_list<T, Allocator>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_forward_list_v = is_std_forward_list<T>::value;

template <typename T>
struct is_std_deque : std::false_type
{
};

template <typename T, typename Allocator>
struct is_std_deque<std::deque<T, Allocator>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_deque_v = is_std_deque<T>::value;

template <typename T>
struct is_std_queue : std::false_type
{
};

template <typename T, typename Container>
struct is_std_queue<std::queue<T, Container>> : std::true_type
{
};

template <typename T>
inline constexpr bool is_std_queue_v = is_std_queue<T>::value;

}  // namespace details

template <typename T>
class SizeChecker
{
    static constexpr void bit_size(const T &aValue) noexcept;
};

namespace details
{
template <typename T, typename P = void>
struct has_min : std::false_type
{
};

template <typename T>
struct has_min<T, std::void_t<decltype(T::kMin)>> : std::true_type
{
};

template <typename T, typename P = void>
struct has_max : std::false_type
{
};

template <typename T>
struct has_max<T, std::void_t<decltype(T::kMax)>> : std::true_type
{
};

template <typename E, bool HasMin, bool HasMax>
struct has_valid_min_max : std::false_type
{
};

template <typename E>
struct has_valid_min_max<E, true, true>
    : std::bool_constant<(E::kMin <= E::kMax) &&
                         (sizeof(E) <= sizeof(std::uint64_t))>
{
};
}  // namespace details

template <typename E>
struct is_min_max_enum
    : std::conjunction<std::is_enum<E>,
                       details::has_valid_min_max<E, details::has_min<E>::value,
                                                  details::has_max<E>::value>>
{
};

template <typename E>
inline constexpr bool is_min_max_enum_v = is_min_max_enum<E>::value;

//----------------------------------------------------------------------
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
    std::enable_if_t<is_min_max_enum_v<utils::remove_cvref_t<T>>, U>;
//----------------------------------------------------------------------

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

    template <typename Writer>
    static constexpr void serialize(Writer &aWriter, T aValue) noexcept
    {
        UIntT valueToSave = static_cast<UIntT>(interval_type::indexOf(aValue));
        aWriter.addValue(valueToSave, NumBits(kNumBits));
    }

    template <typename Reader, typename E>
    static constexpr decltype(auto) deserialize(Reader &aReader,
                                                E &aValue) noexcept
    {
        auto index = aReader.template getValue<UIntT>(NumBits(kNumBits));
        if (index <= interval_type::kMaxIndex)
        {
            aValue = static_cast<E>(interval_type::valueAt(index));
            return Reader::success();
        }
        else
        {
            return Reader::new_error(reader_error::invalid_interval_index);
        }
    }
};
}  // namespace details

template <typename T, typename Reader>
constexpr decltype(auto) deserialize(Reader &aReader, T &aValue,
                                     tag_t<enable_if_u_int_t<T>>) noexcept
{
    aValue = aReader.template getValue<T>();
    return Reader::success();
}

template <typename T, typename Reader>
constexpr decltype(auto) deserialize(Reader &aReader, T &aValue,
                                     tag_t<enable_if_i_or_f_t<T>>) noexcept
{
    using UIntT = utils::uint_from_nbits_t<utils::num_bits<T>()>;
    aValue = utils::bit_cast<T>(aReader.template getValue<UIntT>());
    return Reader::success();
}

template <typename T, typename Reader>
constexpr decltype(auto) deserialize(Reader &aReader, T &aValue,
                                     tag_t<enable_if_bool_t<T>>) noexcept
{
    auto tmp = aReader.template getValue<std::uint8_t>(NumBits(1));
    aValue = static_cast<bool>(tmp);
    return Reader::success();
}

template <typename Reader, typename T>
constexpr decltype(auto) deserialize(Reader &aReader, T &aValue,
                                     tag_t<enable_if_enum_t<T>>) noexcept
{
    using IntervalT =
        interval::Interval<interval::Min<utils::to_underlying(T::eMin)>,
                           interval::Max<utils::to_underlying(T::eMin)>>;
    return details::Interval<IntervalT>::template deserialize(aReader, aValue);
}

template <typename T, typename Writer>
constexpr decltype(auto) serialize(Writer &aWriter, const T aValue,
                                   tag_t<enable_if_u_int_t<T>>) noexcept
{
    aWriter.addValue(std::move(aValue));
    return Writer::success();
}

template <typename T, typename Writer>
constexpr decltype(auto) serialize(Writer &aWriter, const T aValue,
                                   tag_t<enable_if_i_or_f_t<T>>) noexcept
{
    using CoreT = utils::remove_cvref_t<T>;
    using UIntT = utils::UInt<sizeof(CoreT)>;
    UIntT uValue = utils::bit_cast<UIntT>(aValue);
    aWriter.addValue(std::move(uValue));
    return Writer::success();
}

template <typename T, typename Writer>
constexpr decltype(auto) serialize(Writer &aWriter, const T aValue,
                                   tag_t<enable_if_bool_t<T>>) noexcept
{
    auto uValue = static_cast<std::uint8_t>(aValue);
    aWriter.addValue(uValue, NumBits(1));
    return Writer::success();
}

template <typename Writer, typename T>
constexpr decltype(auto) serialize(Writer &aWriter, const T aValue,
                                   tag_t<enable_if_enum_t<T>>) noexcept
{
    using IntervalT =
        interval::Interval<interval::Min<utils::to_underlying(T::eMin)>,
                           interval::Max<utils::to_underlying(T::eMin)>>;
    auto underlyingValue = utils::to_underlying(aValue);
    details::Interval<IntervalT>::template serialize(
        aWriter, std::move(underlyingValue));
    return Writer::success();
}

// enable if bool
template <typename T>
constexpr std::enable_if_t<std::is_same_v<T, bool>, std::size_t>
rabbit_bit_size(tag_t<T>) noexcept
{
    return 1;
}

// enable if signed or unsigned integer, float or double with size <= 64 bits
template <typename T>
constexpr std::enable_if_t<
    std::conjunction_v<std::is_arithmetic<T>,
                       std::negation<std::is_same<T, bool>>,
                       std::bool_constant<sizeof(T) <= sizeof(std::uint64_t)>>,
    std::size_t>
rabbit_bit_size(tag_t<T>) noexcept
{
    return sizeof(T) * static_cast<std::size_t>(CHAR_BIT);
}

template <typename T>
constexpr std::enable_if_t<is_min_max_enum_v<T>, std::size_t> rabbit_bit_size(
    tag_t<T>) noexcept
{
    return utils::bits_count(
        interval::Interval<
            interval::Min<utils::to_underlying(T::eMin)>,
            interval::Max<utils::to_underlying(T::eMax)>>::kMaxIndex);
}

template <typename T>
struct is_container_but_not_array
    : std::conjunction<details::has_size_and_iterable_via_begin_end<T>,
                       std::negation<details::is_std_array<T>>>
{
};

template <typename T>
inline constexpr bool is_container_but_not_array_v =
    is_container_but_not_array<T>::value;

template <typename T>
constexpr std::enable_if_t<is_container_but_not_array_v<T>, std::size_t>
rabbit_bit_size(const T &aValue, tag_t<T>) noexcept;

template <typename T, std::size_t NBits,
          typename = std::enable_if_t<utils::is_uint_v<T>>>
struct size_traits
{
    using value_type = T;
    static constexpr std::size_t descr_bit_size = NBits;
    static constexpr std::size_t max_step_count =
        static_cast<std::size_t>(1 << NBits);
    static constexpr std::size_t bits_per_step =
        utils::num_bits<T>() / max_step_count;
    static constexpr std::size_t min_bit_size = descr_bit_size + bits_per_step;
    static constexpr std::size_t bit_size(T aValue) noexcept
    {
        return descr_bit_size + step_count(aValue) * bits_per_step;
    }

    static constexpr std::size_t step_count(T aValue) noexcept
    {
        if (aValue)
        {
            const std::size_t bsize =
                utils::num_bits<T>() -
                static_cast<std::size_t>(utils::clz(aValue));
            const auto r = utils::div(bsize, bits_per_step);
            return r.quot + (r.rem ? 1 : 0);
        }
        else
        {
            return 1;
        }
    }
};

using default_size_traits = size_traits<std::uint64_t, 4>;

template <typename T>
constexpr std::enable_if_t<is_container_but_not_array_v<T>, std::size_t>
rabbit_bit_size(tag_t<T>) noexcept
{
    return default_size_traits::min_bit_size;
}

CREATE_FREE_FUNCTION_CHECKERS(rabbit_bit_size)

template <typename T, template <typename> class Tag>
struct is_size_defined_by_type;

namespace details
{
template <typename T, template <typename> class Tag>
struct is_size_defined_by_type_single
    : std::conjunction<
          is_rabbit_bit_size_noexcept_invocable_r<std::size_t, Tag<T>>,
          std::negation<
              is_rabbit_bit_size_noexcept_invocable_r<std::size_t, T, Tag<T>>>>
{
};

template <typename T, template <typename> class Tag, bool IsAggregate>
struct is_size_defined_by_type_common;

template <typename T, template <typename> class Tag>
struct is_size_defined_by_type_aggregate
{
    template <std::size_t... I>
    static std::conjunction<
        is_size_defined_by_type<pfr::tuple_element_t<I, T>, Tag>...>
        test(std::index_sequence<I...>);
};

template <typename T, template <typename> class Tag, bool IsAggregate>
struct is_size_defined_by_type_common
    : std::conditional_t<
          is_size_defined_by_type_single<T, Tag>::value, std::true_type,
          decltype(is_size_defined_by_type_aggregate<T, Tag>::test(
              std::make_index_sequence<pfr::tuple_size_v<T>>{}))>
{
};

template <typename T, template <typename> class Tag>
struct is_size_defined_by_type_common<T, Tag, false>
    : is_size_defined_by_type_single<T, Tag>
{
};
}  // namespace details

template <typename T, template <typename> class Tag>
struct is_size_defined_by_type
    : details::is_size_defined_by_type_common<T, Tag, std::is_aggregate_v<T>>
{
};

template <typename T, template <typename> class Tag>
inline constexpr bool is_size_defined_by_type_v =
    is_size_defined_by_type<T, Tag>::value;

//*************************************************************************************//

template <typename T, template <typename> class Tag>
struct size_defined_by_type;

namespace details
{
template <typename T, template <typename> class Tag>
struct size_defined_by_type_single
{
    template <typename U>
    static std::enable_if_t<
        is_rabbit_bit_size_noexcept_invocable_r_v<std::size_t, Tag<U>>,
        std::integral_constant<std::size_t, rabbit_bit_size(Tag<U>{})>>
        test(Tag<U>);

    template <typename U>
    static std::integral_constant<std::size_t, 0> test(...);

    using type = decltype(test<T>(Tag<T>{}));
};

template <typename T, template <typename> class Tag, bool IsAggregate>
struct size_defined_by_type_common;

template <typename T, template <typename> class Tag>
struct size_defined_by_type_aggregate
{
    template <std::size_t... I>
    static std::integral_constant<
        std::size_t,
        (0 + ... +
         size_defined_by_type<pfr::tuple_element_t<I, T>, Tag>::value)>
        test(std::index_sequence<I...>);
};

template <typename T, template <typename> class Tag, bool IsAggregate>
struct size_defined_by_type_common
    : std::conditional_t<is_size_defined_by_type_single<T, Tag>::value,
                         typename size_defined_by_type_single<T, Tag>::type,
                         decltype(size_defined_by_type_aggregate<T, Tag>::test(
                             std::make_index_sequence<pfr::tuple_size_v<T>>{}))>
{
};

template <typename T, template <typename> class Tag>
struct size_defined_by_type_common<T, Tag, false>
    : size_defined_by_type_single<T, Tag>::type
{
};
}  // namespace details

template <typename T, template <typename> class Tag>
struct size_defined_by_type
    : details::size_defined_by_type_common<T, Tag, std::is_aggregate_v<T>>
{
};

template <typename T, template <typename> class Tag>
inline constexpr std::size_t size_defined_by_type_v =
    size_defined_by_type<T, Tag>::value;

template <typename T>
constexpr std::enable_if_t<is_container_but_not_array_v<T>, std::size_t>
rabbit_bit_size(const T &aValue, tag_t<T>) noexcept
{
    using ParamT = typename T::value_type;
    if constexpr (is_size_defined_by_type_v<ParamT, tag_t>)
    {
        return default_size_traits::bit_size(aValue.size()) +
               aValue.size() * rabbit_bit_size(tag<ParamT>);
    }
    else
    {
        std::size_t r = default_size_traits::bit_size(aValue.size());
        for (const auto &v: aValue)
        {
            r += rabbit_bit_size(v);
        }
    }
}

namespace details
{
template <typename T>
using tagged_bit_size_result_t = decltype(bit_size(tag<T>));

template <typename T>
inline constexpr bool is_tagged_bit_size_defined_v =
    utils::is_detected_exact_v<std::size_t, tagged_bit_size_result_t, T>;

template <typename T>
using bit_size_result_t =
    decltype(SizeChecker<T>::bit_size(std::declval<const T &>()));

CREATE_FREE_FUNCTION_CHECKERS(serialize);
CREATE_FREE_FUNCTION_CHECKERS(deserialize);
}  // namespace details

template <typename T, typename Writer>
inline constexpr bool is_serialize_defined_v =
    details::is_serialize_noexcept_invocable_r_v<
        typename Writer::base::result, Writer &, const T &,
        typename Writer::template tag_t<T>>;

template <typename T, typename Reader>
inline constexpr bool is_deserialize_defined_v =
    details::is_deserialize_noexcept_invocable_r_v<
        typename Reader::base::result, Reader &, T &,
        typename Reader::template tag_t<T>>;

template <typename T>
inline constexpr bool is_bit_size_defined_v =
    utils::is_detected_exact_v<std::size_t, details::bit_size_result_t, T>;

template <typename T>
constexpr std::size_t bit_sizeof(const T &aValue) noexcept;

namespace details
{
template <typename T, typename Writer, bool IsAggregate>
struct is_serializable;

template <typename T, typename Reader, bool IsAggregate>
struct is_deserializable;

template <typename T, bool IsAggregate, bool IsTaggedBitSizeDefined>
struct is_compile_time_computable_size;

template <typename T, bool IsAggregate, bool IsTaggedBitSizeDefined>
struct compile_time_bit_size;

template <typename T>
struct aggregate
{
    template <typename Writer, std::size_t... I>
    static std::bool_constant<
        (... && is_serializable<
                    pfr::tuple_element_t<I, T>, Writer,
                    std::is_aggregate_v<pfr::tuple_element_t<I, T>>>::value)>
        isSerializable(std::index_sequence<I...>);

    template <typename Reader, std::size_t... I>
    static std::bool_constant<
        (... && is_deserializable<
                    pfr::tuple_element_t<I, T>, Reader,
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

template <typename T, typename Writer, bool IsAggregate>
struct is_serializable
    : decltype(aggregate<T>::template isSerializable<Writer>(
          std::make_index_sequence<pfr::tuple_size_v<T>>{}))
{
};

template <typename T, typename Writer>
struct is_serializable<T, Writer, false>
    : std::bool_constant<is_serialize_defined_v<T, Writer>>
{
};

template <typename T, typename Reader>
struct is_deserializable<T, Reader, true>
    : decltype(aggregate<T>::template isDeserializable<Reader>(
          std::make_index_sequence<pfr::tuple_size_v<T>>{}))
{
};

template <typename T, typename Reader>
struct is_deserializable<T, Reader, false>
    : std::bool_constant<is_deserialize_defined_v<T, Reader>>
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

template <typename T, typename Writer>
inline constexpr bool is_serializable_v =
    details::is_serializable<T, Writer, std::is_aggregate_v<T>>::value;

template <typename T, typename Reader>
inline constexpr bool is_deserializable_v =
    details::is_deserializable<T, Reader, std::is_aggregate_v<T>>::value;

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
