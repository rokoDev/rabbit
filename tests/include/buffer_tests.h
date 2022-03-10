#ifndef buffer_tests_h
#define buffer_tests_h
#include <fmt/core.h>

#include <string>

#include "data_formatters_tests.h"
#include "rabbit/utils.h"

template <std::size_t BufSize, typename DataT = uint8_t>
class Buffer
{
   public:
    template <typename T, typename... U>
    constexpr Buffer(T &&aFirstArg, U &&... aRestArgs) noexcept
        : array_{rabbit::concatenate_arrays(
              rabbit::make_array(static_cast<DataT>(aFirstArg),
                                 static_cast<DataT>(aRestArgs)...),
              rabbit::make_zero_array<DataT,
                                      BufSize - sizeof...(aRestArgs) - 1>())}
    {
    }

    constexpr Buffer(std::array<DataT, BufSize> &&aArray) noexcept
        : array_{std::move(aArray)}
    {
    }

    void print() const { fmt::print("{}\n", to_string()); }

    Buffer() = default;

    std::string to_string() const noexcept
    {
        return to_string_impl(std::make_index_sequence<BufSize>{});
    }

    constexpr decltype(auto) operator[](std::size_t aPos)
    {
        return array_[aPos];
    }
    constexpr decltype(auto) operator[](std::size_t aPos) const
    {
        return array_[aPos];
    }

    constexpr DataT *data() noexcept { return array_.data(); }

    constexpr DataT const *data() const noexcept { return array_.data(); }

    std::array<DataT, BufSize> array_{};

   private:
    template <std::size_t... I>
    std::string to_string_impl(std::index_sequence<I...>) const noexcept
    {
        return fmt::format(BinFormatStringView<BufSize>, array_[I]...);
    }
};

#endif /* buffer_tests_h */