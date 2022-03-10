#include <fmt/core.h>
#include <gtest/gtest.h>

#include "rabbit/endian.h"

TEST(EndianTest, AssumedEndianEqualActual)
{
    ASSERT_EQ(rabbit::kEndian /*assumed endian*/,
              rabbit::endian() /*actual endian*/);
}