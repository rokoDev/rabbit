#ifndef rabbit_tests_result_adapter_specs_h
#define rabbit_tests_result_adapter_specs_h

#include <rabbit/rabbit.h>

#include <boost/leaf.hpp>

struct leaf_result_adapter
    : rabbit::result_adapter_base<leaf_result_adapter,
                                  boost::leaf::result<void>>
{
   public:
    using result = boost::leaf::result<void>;
    using base = rabbit::result_adapter_base<leaf_result_adapter, result>;
    friend base;

   private:
    template <typename... Args>
    static decltype(auto) new_error(Args &&...aArgs) noexcept
    {
        return result{boost::leaf::new_error(std::forward<Args>(aArgs)...)};
    }

    static decltype(auto) success() noexcept { return result{}; }

    static bool is_success(const result &aResult) noexcept
    {
        return static_cast<bool>(aResult);
    }

    static bool is_error(const result &aResult) noexcept
    {
        return !is_success(aResult);
    }
};

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
