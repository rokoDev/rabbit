#ifndef data_formatters_tests_h
#define data_formatters_tests_h

#include <fmt/core.h>

#include <string>
#include <string_view>

#include "rabbit/utils.h"

using namespace std::string_view_literals;

namespace details
{
template <std::size_t ByteIndex, std::size_t Size, std::size_t... I1,
          std::size_t... I2>
constexpr decltype(auto) format_data_array(const std::string_view aArr1,
                                           std::index_sequence<I1...>,
                                           const std::string_view aArr2,
                                           std::index_sequence<I2...>) noexcept
{
    if constexpr (ByteIndex < Size - 1)
    {
        return rabbit::concatenate_arrays(rabbit::make_array(aArr1[I1]...),
                                          rabbit::make_array(aArr2[I2]...));
    }
    else
    {
        return rabbit::make_array(aArr1[I1]...);
    }
}

template <std::size_t Len1, std::size_t Len2, std::size_t... I,
          std::size_t... BeginIndex, std::size_t... EndIndex>
constexpr decltype(auto) make_fmt_data_array(
    const std::string_view aBegin, const std::string_view aArr1,
    const std::string_view aArr2, const std::string_view aEnd,
    std::index_sequence<I...>, std::index_sequence<BeginIndex...>,
    std::index_sequence<EndIndex...>) noexcept
{
    constexpr std::size_t Size = sizeof...(I);
    using Indices1 = std::make_index_sequence<Len1>;
    using Indices2 = std::make_index_sequence<Len2>;

    return rabbit::concatenate_arrays(
        rabbit::sv_to_array<BeginIndex...>(aBegin),
        format_data_array<I, Size>(aArr1, Indices1{}, aArr2, Indices2{})...,
        rabbit::sv_to_array<EndIndex...>(aEnd));
}

template <std::size_t NumValues, std::size_t kBeginLen,
          std::size_t kOneValueFmtSize, std::size_t kDelimiterFmtSize,
          std::size_t kEndLen>
constexpr auto make_fmt_array(const std::string_view aBegin,
                              const std::string_view aOneValueFmtStr,
                              const std::string_view aDelimiter,
                              const std::string_view aEnd) noexcept
{
    assert(kOneValueFmtSize == aOneValueFmtStr.size());
    assert(kDelimiterFmtSize == aDelimiter.size());
    assert(kBeginLen == aBegin.size());
    assert(kEndLen == aEnd.size());
    using Indices = std::make_index_sequence<NumValues>;
    using BeginIndices = std::make_index_sequence<kBeginLen>;
    using EndIndices = std::make_index_sequence<kEndLen>;
    return details::make_fmt_data_array<kOneValueFmtSize, kDelimiterFmtSize>(
        aBegin, aOneValueFmtStr, aDelimiter, aEnd, Indices{}, BeginIndices{},
        EndIndices{});
}

template <std::size_t NumValues, std::size_t kBeginLen, std::size_t Len1,
          std::size_t Len2, std::size_t kEndLen>
constexpr auto make_fmt_array(const char (&aBegin)[kBeginLen],
                              const char (&aOneValueFmtStr)[Len1],
                              const char (&aDelimiter)[Len2],
                              const char (&aEnd)[kEndLen]) noexcept
{
    constexpr std::size_t kOneValueFmtSize = Len1 - 1;
    const std::string_view kOneValueFmtStr{aOneValueFmtStr, kOneValueFmtSize};

    constexpr std::size_t kDelimiterFmtSize = Len2 - 1;
    const std::string_view kDelimiter{aDelimiter, kDelimiterFmtSize};

    constexpr std::size_t kBeginSize = kBeginLen - 1;
    const std::string_view kBeginStr{aBegin, kBeginSize};

    constexpr std::size_t kEndSize = kEndLen - 1;
    const std::string_view kEndStr{aEnd, kEndSize};

    return make_fmt_array<NumValues, kBeginSize, kOneValueFmtSize,
                          kDelimiterFmtSize, kEndSize>(
        kBeginStr, kOneValueFmtStr, kDelimiter, kEndStr);
}

template <std::size_t NBytes>
struct BinFormatData
{
    static inline constexpr auto kBeginStr = ""sv;
    static inline constexpr auto kOneByteFmtStr = "{:08b}"sv;
    static inline constexpr auto kDelimiterFmtStr = " "sv;
    static inline constexpr auto kEndStr = ""sv;
    static constexpr auto formatDataArray =
        make_fmt_data_array<kOneByteFmtStr.size(), kDelimiterFmtStr.size()>(
            kBeginStr, kOneByteFmtStr, kDelimiterFmtStr, kEndStr,
            std::make_index_sequence<NBytes>{},
            std::make_index_sequence<kBeginStr.size()>{},
            std::make_index_sequence<kEndStr.size()>{});
    static constexpr std::string_view formatStringView{formatDataArray.data(),
                                                       formatDataArray.size()};
};
}  // namespace details

template <std::size_t NBytes>
constexpr std::string_view BinFormatStringView =
    details::BinFormatData<NBytes>::formatStringView;

template <typename Values>
struct Printer;

template <auto... Values>
struct Printer<rabbit::values<Values...>>
{
    static inline constexpr auto kBegin = "values("sv;
    static inline constexpr auto kOneValueFmtStr = "{}"sv;
    static inline constexpr auto kDelimiterFmtStr = ", "sv;
    static inline constexpr auto kEnd = ")\n"sv;
    static constexpr auto formatArray =
        details::make_fmt_array<sizeof...(Values), kBegin.size(),
                                kOneValueFmtStr.size(), kDelimiterFmtStr.size(),
                                kEnd.size()>(kBegin, kOneValueFmtStr,
                                             kDelimiterFmtStr, kEnd);
    static constexpr std::string_view formatSV{formatArray.data(),
                                               formatArray.size()};

    static void execute() noexcept { fmt::print(formatSV, Values...); }
};

template <typename... Types>
struct Printer<rabbit::type_list<Types...>>
{
    static void execute() noexcept { (Printer<Types>::execute(), ...); }
};

template <typename T>
void print()
{
    Printer<T>::execute();
}

#endif /* data_formatters_tests_h */