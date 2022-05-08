#include "get_value_tests.h"

namespace
{
using namespace rabbit::test;

TEST_P(Args3U8, getValue)
{
    // arrange
    using U = uint8_t;
    const auto kExpected = expectedValue<U>();
    const auto [kSrc, kSrcOffset, kNBits] = getArgs();

    // act
    const auto kActual =
        rabbit::BinOps::getValue<U>(Src(kSrc), kSrcOffset, kNBits);

    // assert
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args3U8);

TEST_P(Args3U16, getValue)
{
    // arrange
    using U = uint16_t;
    const auto kExpected = expectedValue<U>();
    const auto [kSrc, kSrcOffset, kNBits] = getArgs();

    // act
    const auto kActual =
        rabbit::BinOps::getValue<U>(Src(kSrc), kSrcOffset, kNBits);

    // assert
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args3U16);

TEST_P(Args3U32, getValue)
{
    // arrange
    using U = uint32_t;
    const auto kExpected = expectedValue<U>();
    const auto [kSrc, kSrcOffset, kNBits] = getArgs();

    // act
    const auto kActual =
        rabbit::BinOps::getValue<U>(Src(kSrc), kSrcOffset, kNBits);

    // assert
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args3U32);

TEST_P(Args3U64, getValue)
{
    // arrange
    using U = uint64_t;
    const auto kExpected = expectedValue<U>();
    const auto [kSrc, kSrcOffset, kNBits] = getArgs();

    // act
    const auto kActual =
        rabbit::BinOps::getValue<U>(Src(kSrc), kSrcOffset, kNBits);

    // assert
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args3U64);

TEST_P(Args2U8, getValue)
{
    // arrange
    using U = uint8_t;
    const auto kExpected = expectedValue<U>();
    const auto [kSrc, kNBits] = getArgs();

    // act
    const auto kActual = rabbit::BinOps::getValue<U>(Src(kSrc), kNBits);

    // assert
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args2U8);

TEST_P(Args2U16, getValue)
{
    // arrange
    using U = uint16_t;
    const auto kExpected = expectedValue<U>();
    const auto [kSrc, kNBits] = getArgs();

    // act
    const auto kActual = rabbit::BinOps::getValue<U>(Src(kSrc), kNBits);

    // assert
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args2U16);

TEST_P(Args2U32, getValue)
{
    // arrange
    using U = uint32_t;
    const auto kExpected = expectedValue<U>();
    const auto [kSrc, kNBits] = getArgs();

    // act
    const auto kActual = rabbit::BinOps::getValue<U>(Src(kSrc), kNBits);

    // assert
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args2U32);

TEST_P(Args2U64, getValue)
{
    // arrange
    using U = uint64_t;
    const auto kExpected = expectedValue<U>();
    const auto [kSrc, kNBits] = getArgs();

    // act
    const auto kActual = rabbit::BinOps::getValue<U>(Src(kSrc), kNBits);

    // assert
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args2U64);

TEST_P(Args1U8, getValue)
{
    // arrange
    using U = uint8_t;
    const auto kExpected = expectedValue<U>();
    const auto [kSrc] = getArgs();

    // act
    const auto kActual = rabbit::BinOps::getValue<U>(Src(kSrc));

    // assert
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args1U8);

TEST_P(Args1U16, getValue)
{
    // arrange
    using U = uint16_t;
    const auto kExpected = expectedValue<U>();
    const auto [kSrc] = getArgs();

    // act
    const auto kActual = rabbit::BinOps::getValue<U>(Src(kSrc));

    // assert
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args1U16);

TEST_P(Args1U32, getValue)
{
    // arrange
    using U = uint32_t;
    const auto kExpected = expectedValue<U>();
    const auto [kSrc] = getArgs();

    // act
    const auto kActual = rabbit::BinOps::getValue<U>(Src(kSrc));

    // assert
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args1U32);

TEST_P(Args1U64, getValue)
{
    // arrange
    using U = uint64_t;
    const auto kExpected = expectedValue<U>();
    const auto [kSrc] = getArgs();

    // act
    const auto kActual = rabbit::BinOps::getValue<U>(Src(kSrc));

    // assert
    static_assert(std::is_same_v<decltype(kActual), decltype(kExpected)>,
                  "kActual has incorrect type.");
    ASSERT_EQ(kActual, kExpected);
}
GTEST_ALLOW_UNINSTANTIATED_PARAMETERIZED_TEST(Args1U64);

}  // namespace
