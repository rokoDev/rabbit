#include <gtest/gtest.h>

#include "add_value_constexpr_tests.h"

namespace
{
#ifdef CORE_V1
using core_t = ::rabbit::v1::Core;
#endif

#ifdef CORE_V2
using core_t = ::rabbit::v2::Core;
#endif

using AddWith4ArgsNB1T = AddWith4ArgsT<core_t, NumBitsT>;
using AddWith3ArgsNB1T = AddWith3ArgsT<core_t, NumBitsT>;
using AddWith2ArgsNB1T = AddWith2ArgsT<core_t, NumBitsT>;

// constexpr tests instantiation for add_value function with 4 arguments
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit4NB1, AddValueCompileTime,
                               AddWith4ArgsNB1T, );

// constexpr tests instantiation for add_value function with 3 arguments
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit3NB1, AddValueCompileTime,
                               AddWith3ArgsNB1T, );

// constexpr tests instantiation for add_value function with 2 arguments
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit2NB1, AddValueCompileTime,
                               AddWith2ArgsNB1T, );

}  // namespace
