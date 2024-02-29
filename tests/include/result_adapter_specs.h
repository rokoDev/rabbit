#ifndef rabbit_tests_result_adapter_specs_h
#define rabbit_tests_result_adapter_specs_h

#include <boost/leaf.hpp>

namespace rabbit
{
template <>
struct result_adapter<boost::leaf::result<void>>
{
    using result_t = boost::leaf::result<void>;

    template <typename... Args>
    static decltype(auto) new_error(Args &&...aArgs) noexcept
    {
        return result_t{boost::leaf::new_error(std::forward<Args>(aArgs)...)};
    }

    template <typename... Args>
    static decltype(auto) success(Args &&...aArgs) noexcept
    {
        return result_t{std::forward<Args>(aArgs)...};
    }

    static bool is_success(const result_t &aResult) noexcept
    {
        return static_cast<bool>(aResult);
    }

    static bool is_error(const result_t &aResult) noexcept
    {
        return !is_success(aResult);
    }
};

template <>
struct result_adapter<reader_error>
{
    using result_t = reader_error;

    template <typename... Args>
    constexpr static decltype(auto) new_error(Args &&...aArgs) noexcept
    {
        return result_t{std::forward<Args>(aArgs)...};
    }

    template <typename... Args>
    constexpr static decltype(auto) success(Args &&...) noexcept
    {
        return result_t{reader_error::success};
    }

    inline constexpr static bool is_success(result_t aResult) noexcept
    {
        return aResult == result_t::success;
    }

    inline constexpr static bool is_error(result_t aResult) noexcept
    {
        return !is_success(aResult);
    }
};

template <>
struct result_adapter<writer_error>
{
    using result_t = writer_error;

    template <typename... Args>
    constexpr static decltype(auto) new_error(Args &&...aArgs) noexcept
    {
        return result_t{std::forward<Args>(aArgs)...};
    }

    template <typename... Args>
    constexpr static decltype(auto) success(Args &&...) noexcept
    {
        return result_t{writer_error::success};
    }

    inline constexpr static bool is_success(result_t aResult) noexcept
    {
        return aResult == result_t::success;
    }

    inline constexpr static bool is_error(result_t aResult) noexcept
    {
        return !is_success(aResult);
    }
};
}  // namespace rabbit

template <typename T>
struct tag_t
{
};

template <typename T>
constexpr std::enable_if_t<
    std::conjunction_v<std::is_arithmetic<T>,
                       std::negation<std::is_same<T, bool>>,
                       std::bool_constant<sizeof(T) <= sizeof(std::uint64_t)>>,
    std::size_t>
rabbit_bit_size(tag_t<T>) noexcept
{
    return utils::num_bits<T>();
}

#endif /* rabbit_tests_result_adapter_specs_h */
