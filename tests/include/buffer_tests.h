#ifndef buffer_tests_h
#define buffer_tests_h

#include <charconv>
#include <iostream>
#include <string>

#include "rabbit/utils.h"

template <std::size_t BufSize, typename DataT = uint8_t>
class Buffer
{
   public:
    template <typename T, typename... U>
    constexpr Buffer(T &&aFirstArg, U &&...aRestArgs) noexcept
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

    void print() const { std::cout << to_string() << '\n'; }

    Buffer() = default;

    std::string_view to_string() const noexcept
    {
        auto buf = strData_.data();
        for (const auto aValue: array_)
        {
            if (buf != strData_.data())
            {
                *buf = delimiter_;
                ++buf;
            }
            buf = std::to_chars(buf, buf + CHAR_BIT, aValue, 2);
        }
        return {strData_.data(), strData_.size()};
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
    static inline constexpr char delimiter_ = ' ';
    std::array<char, BufSize * CHAR_BIT + BufSize - 1> strData_{};
};

#endif /* buffer_tests_h */
