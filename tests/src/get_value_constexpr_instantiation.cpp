#include <gtest/gtest.h>

#include "get_value_constexpr_tests.h"

namespace
{
#ifdef CORE_V1
using core_t = ::rabbit::v1::core;
#endif

#ifdef CORE_V2
using core_t = ::rabbit::v2::core;
#endif

// constexpr tests instantiation for get_value function with std::uint8_t
// argument
using NumBits8T = utils::value_list<1_uz, 4_uz, 7_uz, 8_uz>;
using GetWith3ArgsU8 = GetWith3ArgsT<core_t, std::uint8_t, NumBits8T>;
using GetWith2ArgsU8 = GetWith2ArgsT<core_t, std::uint8_t, NumBits8T>;
using GetWith1ArgsU8 = GetWith1ArgsT<core_t, std::uint8_t>;

INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit3U8, GetValueCompileTime,
                               GetWith3ArgsU8, );
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit2U8, GetValueCompileTime,
                               GetWith2ArgsU8, );
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit1U8, GetValueCompileTime,
                               GetWith1ArgsU8, );

// constexpr tests instantiation for get_value function with std::uint16_t
// argument
using NumBits16T = utils::value_list<1_uz, 3_uz, 12_uz, 15_uz, 16_uz>;
using GetWith3ArgsU16 = GetWith3ArgsT<core_t, std::uint16_t, NumBits16T>;
using GetWith2ArgsU16 = GetWith2ArgsT<core_t, std::uint16_t, NumBits16T>;
using GetWith1ArgsU16 = GetWith1ArgsT<core_t, std::uint16_t>;

INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit3U16, GetValueCompileTime,
                               GetWith3ArgsU16, );
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit2U16, GetValueCompileTime,
                               GetWith2ArgsU16, );
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit1U16, GetValueCompileTime,
                               GetWith1ArgsU16, );

// constexpr tests instantiation for get_value function with std::uint32_t
// argument
using NumBits32T = utils::value_list<1_uz, 16_uz, 24_uz, 31_uz, 32_uz>;
using GetWith3ArgsU32 = GetWith3ArgsT<core_t, std::uint32_t, NumBits32T>;
using GetWith2ArgsU32 = GetWith2ArgsT<core_t, std::uint32_t, NumBits32T>;
using GetWith1ArgsU32 = GetWith1ArgsT<core_t, std::uint32_t>;

INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit3U32, GetValueCompileTime,
                               GetWith3ArgsU32, );
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit2U32, GetValueCompileTime,
                               GetWith2ArgsU32, );
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit1U32, GetValueCompileTime,
                               GetWith1ArgsU32, );

// constexpr tests instantiation for get_value function with std::uint64_t
// argument
using NumBits64T = utils::value_list<1_uz, 7_uz, 8_uz, 43_uz, 63_uz, 64_uz>;
using GetWith3ArgsU64 = GetWith3ArgsT<core_t, std::uint64_t, NumBits64T>;
using GetWith2ArgsU64 = GetWith2ArgsT<core_t, std::uint64_t, NumBits64T>;
using GetWith1ArgsU64 = GetWith1ArgsT<core_t, std::uint64_t>;

INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit3U64, GetValueCompileTime,
                               GetWith3ArgsU64, );
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit2U64, GetValueCompileTime,
                               GetWith2ArgsU64, );
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit1U64, GetValueCompileTime,
                               GetWith1ArgsU64, );

}  // namespace
