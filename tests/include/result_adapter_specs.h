#ifndef rabbit_tests_result_adapter_specs_h
#define rabbit_tests_result_adapter_specs_h

#include <rabbit/rabbit.h>

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
