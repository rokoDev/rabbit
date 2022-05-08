#include <gtest/gtest.h>

#include "copy_bits_constexpr_tests.h"

namespace
{
using namespace rabbit::test;
using ::testing::Combine;
using ::testing::Values;
using ::testing::ValuesIn;

using CopyBitsWith5ArgsCompileTimeArgs =
    copy_bits_types_t<7_uz, 8_uz, 9_uz, 63_uz, 64_uz, 70_uz>;
// constexpr copyBits tests instantiation
INSTANTIATE_TYPED_TEST_SUITE_P(Rabbit, CopyBitsCompileTime,
                               CopyBitsWith5ArgsCompileTimeArgs, );
}  // namespace
