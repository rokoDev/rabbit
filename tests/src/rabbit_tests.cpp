#include <gtest/gtest.h>

#include "rabbit/rabbit.h"

TEST(RabbitTests, Constructor)
{
    const auto result = rabbit::BinOps2::plus5(10);
    ASSERT_EQ(result, 15);
}
