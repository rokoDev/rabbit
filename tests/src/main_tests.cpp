#include <fmt/core.h>
#include <gtest/gtest.h>

#include "rabbit/version_info.h"

int main(int argc, char **argv)
{
    ::testing::InitGoogleTest(&argc, argv);

    const auto testResult = RUN_ALL_TESTS();

    fmt::print("{}", rabbit::version_info());

    return testResult;
}