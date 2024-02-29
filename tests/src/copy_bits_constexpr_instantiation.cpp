#include <gtest/gtest.h>

#include "copy_bits_constexpr_tests.h"

namespace
{
#ifdef CORE_V1
using core_t = ::rabbit::v1::core;
#endif

#ifdef CORE_V2
using core_t = ::rabbit::v2::core;
#endif

using CopyWith5ArgsNB1T = CopyWith5ArgsT<core_t, NumBits1T>;
using CopyWith4ArgsNB1T = CopyWith4ArgsT<core_t, NumBits1T>;
using CopyWith3ArgsNB1T = CopyWith3ArgsT<core_t, NumBits1T>;

// constexpr tests instantiation for copy function with 5 arguments
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit5NB1, CopyBitsCompileTime,
                               CopyWith5ArgsNB1T, );

// constexpr tests instantiation for copy function with 4 arguments
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit4NB1, CopyBitsCompileTime,
                               CopyWith4ArgsNB1T, );

// constexpr tests instantiation for copy function with 3 arguments
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit3NB1, CopyBitsCompileTime,
                               CopyWith3ArgsNB1T, );

using CopyWith5ArgsNB2T = CopyWith5ArgsT<core_t, NumBits2T>;
using CopyWith4ArgsNB2T = CopyWith4ArgsT<core_t, NumBits2T>;
using CopyWith3ArgsNB2T = CopyWith3ArgsT<core_t, NumBits2T>;

// constexpr tests instantiation for copy function with 5 arguments
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit5NB2, CopyBitsCompileTime,
                               CopyWith5ArgsNB2T, );

// constexpr tests instantiation for copy function with 4 arguments
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit4NB2, CopyBitsCompileTime,
                               CopyWith4ArgsNB2T, );

// constexpr tests instantiation for copy function with 3 arguments
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit3NB2, CopyBitsCompileTime,
                               CopyWith3ArgsNB2T, );
}  // namespace
