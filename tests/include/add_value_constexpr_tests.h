#ifndef add_value_constexpr_tests_h
#define add_value_constexpr_tests_h
#include <gtest/gtest.h>
#include <utils/utils.h>

#include <string>
#include <string_view>
#include <utility>

#include "rabbit/bin_ops.h"
#include "rabbit/details.h"
#include "test_helpers.h"

namespace
{
using namespace std::string_view_literals;

template <typename T>
class AddValueCompileTime;

template <decltype(auto) DstArr, std::size_t DstIndex, auto DOffset, auto Value,
          auto NBits, typename CoreT>
class AddValueCompileTime<utils::type_list<
    utils::value_list<DstArr, DstIndex, DOffset, Value, NBits>, CoreT>>
    : public ::testing::Test
{
    using DstOffset = ::rabbit::DstOffset;
    using NumBits = ::rabbit::NumBits;
    using bit_helpers = ::test_utils::bits<CoreT>;
    using Dst = ::rabbit::Dst;

   protected:
    static inline constexpr NumBits kNBits{NBits};
    static inline constexpr DstOffset kDstOffset{DOffset};
    static inline constexpr auto kDstBitStr = (*DstArr)[DstIndex];

    static constexpr decltype(auto) expected() noexcept
    {
        return bit_helpers::template addValueExpected<kDstBitStr.size()>(
            kDstBitStr, kDstOffset, Value, kNBits);
    }

    static constexpr decltype(auto) actual() noexcept
    {
        auto dstArr =
            bit_helpers::template to_byte_array<kDstBitStr.size()>(kDstBitStr);
        CoreT::add_value(Dst{dstArr.data()}, kDstOffset, Value, kNBits);
        return bit_helpers::to_symbol_array(dstArr);
    }
};

template <decltype(auto) DstArr, std::size_t DstIndex, auto Value, auto NBits,
          typename CoreT>
class AddValueCompileTime<
    utils::type_list<utils::value_list<DstArr, DstIndex, Value, NBits>, CoreT>>
    : public ::testing::Test
{
    using NumBits = ::rabbit::NumBits;
    using bit_helpers = ::test_utils::bits<CoreT>;
    using Dst = ::rabbit::Dst;

   protected:
    static inline constexpr NumBits kNBits{NBits};
    static inline constexpr auto kDstBitStr = (*DstArr)[DstIndex];

    static constexpr decltype(auto) expected() noexcept
    {
        constexpr ::rabbit::DstOffset kDstOffset{0};
        return bit_helpers::template addValueExpected<kDstBitStr.size()>(
            kDstBitStr, kDstOffset, Value, kNBits);
    }

    static constexpr decltype(auto) actual() noexcept
    {
        auto dstArr =
            bit_helpers::template to_byte_array<kDstBitStr.size()>(kDstBitStr);
        CoreT::add_value(Dst{dstArr.data()}, Value, kNBits);
        return bit_helpers::to_symbol_array(dstArr);
    }
};

template <decltype(auto) DstArr, std::size_t DstIndex, auto Value,
          typename CoreT>
class AddValueCompileTime<
    utils::type_list<utils::value_list<DstArr, DstIndex, Value>, CoreT>>
    : public ::testing::Test
{
    using bit_helpers = ::test_utils::bits<CoreT>;

   protected:
    static inline constexpr auto kDstBitStr = (*DstArr)[DstIndex];

    static constexpr decltype(auto) expected() noexcept
    {
        using DstOffset = ::rabbit::DstOffset;
        using NumBits = ::rabbit::NumBits;
        constexpr NumBits kNBits{utils::num_bits<decltype(Value)>()};
        return bit_helpers::template addValueExpected<kDstBitStr.size()>(
            kDstBitStr, DstOffset{0}, Value, kNBits);
    }

    static constexpr decltype(auto) actual() noexcept
    {
        using Dst = ::rabbit::Dst;
        auto dstArr =
            bit_helpers::template to_byte_array<kDstBitStr.size()>(kDstBitStr);
        CoreT::add_value(Dst{dstArr.data()}, Value);
        return bit_helpers::to_symbol_array(dstArr);
    }
};

// Data to test
template <typename TypeList>
struct testing_type;

template <typename... Ts>
struct testing_type<utils::type_list<Ts...>>
{
    using type = ::testing::Types<Ts...>;
};

template <typename TypeList>
using testing_type_t = typename testing_type<TypeList>::type;

template <typename TypeList, typename... ToMixIn>
struct mixed_in_types;

template <typename... Ts, typename... ToMixIn>
struct mixed_in_types<utils::type_list<Ts...>, ToMixIn...>
{
    using type = utils::type_list<utils::type_list<Ts, ToMixIn...>...>;
};

template <typename TypeList, typename... ToMixIn>
using mixed_in_types_t = typename mixed_in_types<TypeList, ToMixIn...>::type;

using dst_offset_t = rabbit::DstOffset::value_type;
using src_offset_t = rabbit::SrcOffset::value_type;

template <typename ArrayT>
struct index_list;

template <typename T, std::size_t Size>
struct index_list<std::array<T, Size>>
{
   private:
    template <std::size_t... I>
    static utils::value_list<I...> index_list_impl(std::index_sequence<I...>);

   public:
    using type = decltype(index_list_impl(std::make_index_sequence<Size>{}));
};

template <typename ArrayT>
using index_list_t = typename index_list<ArrayT>::type;

template <typename CoreT, auto* DstArr, typename... ValueListsToMix>
struct add_value_types
{
    using Combinations = typename utils::cartesian_product<
        utils::value_list<DstArr>,
        index_list_t<
            utils::remove_cvref_t<std::remove_pointer_t<decltype(DstArr)>>>,
        ValueListsToMix...>::type;
    using type = testing_type_t<mixed_in_types_t<Combinations, CoreT>>;
};

template <typename CoreT, auto* DstArr, typename... ValueListsToMix>
using add_value_types_t =
    typename add_value_types<CoreT, DstArr, ValueListsToMix...>::type;

inline constexpr auto DstArray = utils::make_array(
    "110101101111010110110110101110101001110011101011000000000101100010000101"sv,
    "100110011101101000101011001000010100110010110011011111000101010011110101"sv);

using DstOffsetsT =
    utils::values_in_range_t<static_cast<dst_offset_t>(0),
                             static_cast<dst_offset_t>(CHAR_BIT - 1)>;

using ValuesT = utils::value_list<
    0b01011010'10000010'01101100'01011010'00100101'10101010'10000101'10011101_u64,
    0b11001010'01101011'01100011'11000101'01110001'01110000'00010101'11001010_u64,
    0b11110011'10010110'01011010'01011101'00110001'01000100'10110111'11011011_u64>;

using NumBitsT = utils::value_list<1_uz, 7_uz, 8_uz, 43_uz, 63_uz, 64_uz>;

template <typename CoreT, typename NBitsT>
using AddWith4ArgsT =
    add_value_types_t<CoreT, &DstArray, DstOffsetsT, ValuesT, NBitsT>;

template <typename CoreT, typename NBitsT>
using AddWith3ArgsT = add_value_types_t<CoreT, &DstArray, ValuesT, NBitsT>;

template <typename CoreT, typename NBitsT>
using AddWith2ArgsT = add_value_types_t<CoreT, &DstArray, ValuesT>;

TYPED_TEST_SUITE_P(AddValueCompileTime);

TYPED_TEST_P(AddValueCompileTime, WithArgs)
{
    static constexpr auto kActualArr = TestFixture::actual();
    constexpr std::string_view kActual(kActualArr.data(), kActualArr.size());
    static constexpr auto kExpectedArr = TestFixture::expected();
    constexpr std::string_view kExpected(kExpectedArr.data(),
                                         kExpectedArr.size());
    static_assert(kActual == kExpected);
}

REGISTER_TYPED_TEST_SUITE_P(AddValueCompileTime, WithArgs);
}  // namespace

#endif /* add_value_constexpr_tests_h */
