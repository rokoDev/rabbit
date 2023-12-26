#ifndef get_value_constexpr_tests_h
#define get_value_constexpr_tests_h

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
class GetValueCompileTime;

template <decltype(auto) SrcArr, std::size_t SrcIndex, auto SOffset, auto NBits,
          typename CoreT, typename ValueT>
class GetValueCompileTime<utils::type_list<
    utils::value_list<SrcArr, SrcIndex, SOffset, NBits>, CoreT, ValueT>>
    : public ::testing::Test
{
    using SrcOffset = ::rabbit::SrcOffset;
    using NumBits = ::rabbit::NumBits;
    using bit_helpers = ::test_utils::bits<CoreT>;
    using Src = ::rabbit::Src;

   protected:
    static inline constexpr NumBits kNBits{NBits};
    static inline constexpr SrcOffset kSrcOffset{SOffset};
    static inline constexpr auto kSrcBitStr = (*SrcArr)[SrcIndex];

    static constexpr decltype(auto) expected() noexcept
    {
        return bit_helpers::template getValueExpected<ValueT>(
            kSrcBitStr, kSrcOffset, kNBits);
    }

    static constexpr decltype(auto) actual() noexcept
    {
        auto srcArr =
            bit_helpers::template to_byte_array<kSrcBitStr.size()>(kSrcBitStr);
        return CoreT::template get_value<ValueT>(Src{srcArr.data()}, kSrcOffset,
                                                 kNBits);
    }
};

template <decltype(auto) SrcArr, std::size_t SrcIndex, auto NBits,
          typename CoreT, typename ValueT>
class GetValueCompileTime<
    utils::type_list<utils::value_list<SrcArr, SrcIndex, NBits>, CoreT, ValueT>>
    : public ::testing::Test
{
    using NumBits = ::rabbit::NumBits;
    using bit_helpers = ::test_utils::bits<CoreT>;
    using Src = ::rabbit::Src;

   protected:
    static inline constexpr NumBits kNBits{NBits};
    static inline constexpr auto kSrcBitStr = (*SrcArr)[SrcIndex];

    static constexpr decltype(auto) expected() noexcept
    {
        constexpr ::rabbit::SrcOffset kSrcOffset{0};
        return bit_helpers::template getValueExpected<ValueT>(
            kSrcBitStr, kSrcOffset, kNBits);
    }

    static constexpr decltype(auto) actual() noexcept
    {
        auto srcArr =
            bit_helpers::template to_byte_array<kSrcBitStr.size()>(kSrcBitStr);
        return CoreT::template get_value<ValueT>(Src{srcArr.data()}, kNBits);
    }
};

template <decltype(auto) SrcArr, std::size_t SrcIndex, typename CoreT,
          typename ValueT>
class GetValueCompileTime<
    utils::type_list<utils::value_list<SrcArr, SrcIndex>, CoreT, ValueT>>
    : public ::testing::Test
{
    using bit_helpers = ::test_utils::bits<CoreT>;

   protected:
    static inline constexpr auto kSrcBitStr = (*SrcArr)[SrcIndex];

    static constexpr decltype(auto) expected() noexcept
    {
        using SrcOffset = ::rabbit::SrcOffset;
        using NumBits = ::rabbit::NumBits;
        constexpr NumBits kNBits{utils::num_bits<ValueT>()};
        return bit_helpers::template getValueExpected<ValueT>(
            kSrcBitStr, SrcOffset{0}, kNBits);
    }

    static constexpr decltype(auto) actual() noexcept
    {
        using Src = ::rabbit::Src;
        auto srcArr =
            bit_helpers::template to_byte_array<kSrcBitStr.size()>(kSrcBitStr);
        return CoreT::template get_value<ValueT>(Src{srcArr.data()});
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

template <typename CoreT, typename ValueT, auto* SrcArr,
          typename... ValueListsToMix>
struct get_value_types
{
    using Combinations = typename utils::cartesian_product<
        utils::value_list<SrcArr>,
        index_list_t<
            utils::remove_cvref_t<std::remove_pointer_t<decltype(SrcArr)>>>,
        ValueListsToMix...>::type;
    using type = testing_type_t<mixed_in_types_t<Combinations, CoreT, ValueT>>;
};

template <typename CoreT, typename ValueT, auto* SrcArr,
          typename... ValueListsToMix>
using get_value_types_t =
    typename get_value_types<CoreT, ValueT, SrcArr, ValueListsToMix...>::type;

inline constexpr auto SrcArray = utils::make_array(
    "110101101111010110110110101110101001110011101011000000000101100010000101"sv,
    "100110011101101000101011001000010100110010110011011111000101010011110101"sv);

using SrcOffsetsT =
    utils::values_in_range_t<static_cast<src_offset_t>(0),
                             static_cast<src_offset_t>(CHAR_BIT - 1)>;

template <typename CoreT, typename ValueT, typename NBitsT>
using GetWith3ArgsT =
    get_value_types_t<CoreT, ValueT, &SrcArray, SrcOffsetsT, NBitsT>;

template <typename CoreT, typename ValueT, typename NBitsT>
using GetWith2ArgsT = get_value_types_t<CoreT, ValueT, &SrcArray, NBitsT>;

template <typename CoreT, typename ValueT>
using GetWith1ArgsT = get_value_types_t<CoreT, ValueT, &SrcArray>;

TYPED_TEST_SUITE_P(GetValueCompileTime);

TYPED_TEST_P(GetValueCompileTime, WithArgs)
{
    static constexpr auto kActual = TestFixture::actual();
    static constexpr auto kExpected = TestFixture::expected();
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>);
    static_assert(kActual == kExpected);
}

REGISTER_TYPED_TEST_SUITE_P(GetValueCompileTime, WithArgs);
}  // namespace

#endif /* get_value_constexpr_tests_h */
