#ifndef ndt_utils_h
#define ndt_utils_h

#include <fmt/core.h>
#include <fmt/format.h>

#include <array>
#include <chrono>
#include <climits>
#include <cstring>
#include <system_error>
#include <type_traits>
#include <unordered_map>

namespace ndt
{
template <typename T>
struct is_std_array : std::false_type
{
};

template <typename DataT, std::size_t Size>
struct is_std_array<std::array<DataT, Size>> : std::true_type
{
};

template <class T>
struct remove_cvref
{
    typedef std::remove_cv_t<std::remove_reference_t<T>> type;
};

template <class T>
using remove_cvref_t = typename remove_cvref<T>::type;

template <typename T>
inline constexpr bool is_std_array_v = is_std_array<std::decay_t<T>>::value;

template <typename T>
inline constexpr std::size_t std_array_size_v =
    std::tuple_size_v<std::decay_t<T>>;

template <typename T>
using std_array_data_t = typename std::tuple_element_t<0, std::decay_t<T>>;

template <typename A1, typename A2>
inline constexpr bool is_same_data_std_arrays =
    is_std_array_v<A1>&& is_std_array_v<A2>&&
        std::is_same_v<std_array_data_t<A1>, std_array_data_t<A2>>;

template <typename... Arrays>
inline constexpr std::size_t std_array_sizes_sum = (0 + ... +
                                                    std_array_size_v<Arrays>);

namespace details
{
template <typename A1, typename A2, std::size_t... i1, std::size_t... i2,
          typename = std::enable_if_t<is_same_data_std_arrays<A1, A2>>>
static constexpr auto concatenate_2_arrays(A1&& aArray1, A2&& aArray2,
                                           std::index_sequence<i1...>,
                                           std::index_sequence<i2...>) noexcept
    -> std::array<std_array_data_t<A1>, std_array_sizes_sum<A1, A2>>
{
    return {std::get<i1>(std::forward<A1>(aArray1))...,
            std::get<i2>(std::forward<A2>(aArray2))...};
}

template <typename T, typename = std::enable_if_t<is_std_array_v<T>>>
constexpr decltype(auto) concatenate_arrays(T&& aArray) noexcept
{
    return std::forward<T>(aArray);
}

template <typename A1, typename A2, typename... RestArrays>
constexpr auto concatenate_arrays(A1&& aArray1, A2&& aArray2,
                                  RestArrays&&... aArrays) noexcept
    -> std::array<std_array_data_t<A1>,
                  std_array_sizes_sum<A1, A2, RestArrays...>>
{
    using Indices1 = std::make_index_sequence<std_array_size_v<A1>>;
    using Indices2 = std::make_index_sequence<std_array_size_v<A2>>;
    return concatenate_arrays(
        details::concatenate_2_arrays(std::forward<A1>(aArray1),
                                      std::forward<A2>(aArray2), Indices1{},
                                      Indices2{}),
        std::forward<RestArrays>(aArrays)...);
}

template <std::size_t... I>
constexpr uint8_t numBitSet(uint8_t aValue, std::index_sequence<I...>) noexcept
{
    return (... + ((aValue >> I) & 1));
}

constexpr uint8_t getNumBits(uint8_t aValue)
{
    using Indices = std::make_index_sequence<CHAR_BIT>;
    return details::numBitSet(aValue, Indices{});
}

template <std::size_t... I>
constexpr std::array<uint8_t, sizeof...(I)> make_num_bits_table_impl(
    std::index_sequence<I...>) noexcept
{
    return {(getNumBits(I))...};
}

constexpr auto make_num_bits_table() noexcept
{
    using Indices =
        std::make_index_sequence<std::numeric_limits<uint8_t>::max() + 1>;
    return make_num_bits_table_impl(Indices{});
}
}  //  namespace details

template <typename Array, typename... RestArrays>
constexpr auto concatenate_arrays(Array&& aArray,
                                  RestArrays&&... aArrays) noexcept
    -> std::array<std_array_data_t<Array>,
                  std_array_sizes_sum<Array, RestArrays...>>
{
    return details::concatenate_arrays(std::forward<Array>(aArray),
                                       std::forward<RestArrays>(aArrays)...);
}

template <typename... T>
constexpr auto make_array(T&&... aArgs) -> std::array<
    typename std::decay<typename std::common_type<T...>::type>::type,
    sizeof...(T)>
{
    return {std::forward<T>(aArgs)...};
}

inline constexpr auto kNumBitsTable = details::make_num_bits_table();

namespace utils
{
bool memvcmp(const void* memptr, unsigned char val, const std::size_t size);

constexpr uint8_t bits_count(const uint64_t aValue)
{
    return (aValue == 0) ? 0 : 1 + bits_count(aValue >> 1);
}

template <uint8_t BitsCount>
struct uint_from_nbits
{
   private:
    static constexpr auto getType()
    {
        if constexpr ((BitsCount > 0) && (BitsCount <= CHAR_BIT))
        {
            return uint8_t();
        }
        else if constexpr ((BitsCount > CHAR_BIT) &&
                           (BitsCount <= CHAR_BIT * sizeof(uint16_t)))
        {
            return uint16_t();
        }
        else if constexpr ((BitsCount > CHAR_BIT * sizeof(uint16_t)) &&
                           (BitsCount <= CHAR_BIT * sizeof(uint32_t)))
        {
            return uint32_t();
        }
        else if constexpr ((BitsCount > CHAR_BIT * sizeof(uint32_t)) &&
                           (BitsCount <= CHAR_BIT * sizeof(uint64_t)))
        {
            return uint64_t();
        }
        else
        {
            static_assert(BitsCount > 0, "BitsCount must be more than zero");
            static_assert(BitsCount < sizeof(uint64_t) * CHAR_BIT,
                          "BitsCount count can't be more than 64");
            return uint8_t();
        }
    }

   public:
    using type = decltype(getType());
};

template <uint8_t BitsCount>
using uint_from_nbits_t = typename uint_from_nbits<BitsCount>::type;

template <uint8_t BytesCount>
using uint_from_nbytes_t = utils::uint_from_nbits_t<BytesCount * CHAR_BIT>;

template <typename T>
struct enum_properties
{
    static constexpr uint8_t numBits =
        bits_count(static_cast<uint64_t>(T::Count));
    using SerializeT = uint_from_nbits_t<numBits>;
};

template <uint8_t BitsCount>
struct fast_uint_from_nbits
{
   private:
    static constexpr decltype(auto) getType()
    {
        if constexpr ((BitsCount > 0) && (BitsCount <= CHAR_BIT))
        {
            return uint_fast8_t{};
        }
        else if constexpr ((BitsCount > CHAR_BIT) &&
                           (BitsCount <= CHAR_BIT * sizeof(uint16_t)))
        {
            return uint_fast16_t{};
        }
        else if constexpr ((BitsCount > CHAR_BIT * sizeof(uint16_t)) &&
                           (BitsCount <= CHAR_BIT * sizeof(uint32_t)))
        {
            return uint_fast32_t{};
        }
        else if constexpr ((BitsCount > CHAR_BIT * sizeof(uint32_t)) &&
                           (BitsCount <= CHAR_BIT * sizeof(uint64_t)))
        {
            return uint_fast64_t{};
        }
        else
        {
            static_assert(BitsCount > 0, "BitsCount must be more than zero");
            static_assert(BitsCount < sizeof(uint64_t) * CHAR_BIT,
                          "BitsCount count can't be more than 64");
            return uint8_t{};
        }
    }

   public:
    using type = decltype(getType());
};

template <uint8_t BitsCount>
using fast_uint_from_nbits_t = typename fast_uint_from_nbits<BitsCount>::type;

template <uint8_t BytesCount>
using fast_uint_from_nbytes_t =
    typename fast_uint_from_nbits<BytesCount * CHAR_BIT>::type;

template <typename T>
constexpr std::size_t num_bits()
{
    return CHAR_BIT * sizeof(T);
}

template <>
constexpr std::size_t num_bits<bool>()
{
    return 1;
}

template <typename... Ts>
constexpr std::size_t sum_size()
{
    constexpr std::size_t bits_size = (num_bits<Ts>() + ...);
    return bits_size / CHAR_BIT + static_cast<bool>(bits_size % CHAR_BIT);
}

template <typename F, typename... Ts>
void for_each_arg(F&& f, Ts&&... ts)
{
    using I = std::initializer_list<int>;
    (void)I{(std::forward<F>(f)(std::forward<Ts>(ts)), 0)...};
}

template <typename F, typename T>
void for_each_in_tuple(F&& f, T&& t)
{
    std::apply(
        [&f](auto&&... xs) {
            for_each_arg(f, std::forward<decltype(xs)>(xs)...);
        },
        std::forward<T>(t));
}

template <class To, class From>
typename std::enable_if_t<sizeof(To) == sizeof(From) &&
                              std::is_trivially_copyable_v<From> &&
                              std::is_trivially_copyable_v<To>,
                          To>
bit_cast(const From& src) noexcept
{
    static_assert(std::is_trivially_constructible_v<To>,
                  "This implementation additionally requires destination type "
                  "to be trivially constructible");

    To dst;
    std::memcpy(&dst, &src, sizeof(To));
    return dst;
}

template <typename E>
constexpr auto to_underlying(E e) noexcept
{
    return static_cast<typename std::underlying_type_t<E>>(e);
}
}  // namespace utils

template <uint8_t NBytes>
using UInt = utils::uint_from_nbytes_t<NBytes>;

template <std::size_t NBytes>
using FastUInt = utils::fast_uint_from_nbytes_t<NBytes>;

template <std::size_t N, typename Seq>
struct shifted_sequence;

template <std::size_t N, std::size_t... I>
struct shifted_sequence<N, std::index_sequence<I...>>
{
    using type = std::index_sequence<I + N...>;
};
template <std::size_t N, typename Seq>
using shifted_sequence_t = typename shifted_sequence<N, Seq>::type;
}  // namespace ndt

#endif /* ndt_utils_h */