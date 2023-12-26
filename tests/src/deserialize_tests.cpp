#include <gtest/gtest.h>
#include <rabbit/deserialize.h>
#include <user_literals/user_literals.h>

#include "serialization_tests.h"

namespace rabbit
{
template <typename T>
using enable_if_uint16_t =
    std::enable_if_t<std::is_same_v<std::decay_t<T>, std::uint16_t>, T>;

template <typename T>
result<void> deserialize(reader &aReader, T &aValue,
                         tag_t<enable_if_uint16_t<T>>) noexcept
{
    BOOST_LEAF_ASSIGN(aValue, aReader.getValue<T>());
    return {};
}
}  // namespace rabbit

using DeserializeTests64 = Data<std::byte, 64>;
using DeserializeTests1 = Data<std::byte, 1>;

TEST(DeserializeTests, StaticAsserts)
{
    static_assert(rabbit::is_deserializable_v<std::uint16_t>,
                  "std::uint16_t must be deserializable.");
    static_assert(
        rabbit::is_deserialize_defined_v<std::uint16_t>,
        "result<std::uint16_t> deserialize(reader&, tag_t<std::uint16_t>) "
        "noexcept must be defined.");

    static_assert(rabbit::is_deserializable_v<Empty>,
                  "Empty must be deserializable.");
    static_assert(not rabbit::is_deserialize_defined_v<Empty>,
                  "result<Empty> deserialize(reader&, tag_t<Empty>) noexcept "
                  "must be not defined.");

    static_assert(rabbit::is_deserializable_v<NotEmpty>,
                  "NotEmpty must be deserializable.");
    static_assert(not rabbit::is_deserialize_defined_v<NotEmpty>,
                  "result<NotEmpty> deserialize(reader&, tag_t<NotEmpty>) "
                  "noexcept must be not defined.");

    static_assert(rabbit::is_deserializable_v<Nested>,
                  "Nested must be deserializable.");
    static_assert(not rabbit::is_deserialize_defined_v<Nested>,
                  "result<Nested> deserialize(reader&, tag_t<Nested>) noexcept "
                  "must be not defined.");

    static_assert(not rabbit::is_deserializable_v<Unsupported>,
                  "Unsupported must be not deserializable.");
    static_assert(not rabbit::is_deserialize_defined_v<Unsupported>,
                  "result<Unsupported> deserialize(reader&, "
                  "tag_t<Unsupported>) noexcept must be not defined.");

    static_assert(not rabbit::is_deserializable_v<NestedWithUnsupported>,
                  "NestedWithUnsupported must be not deserializable.");
    static_assert(
        not rabbit::is_deserialize_defined_v<NestedWithUnsupported>,
        "result<NestedWithUnsupported> deserialize(reader&, "
        "tag_t<NestedWithUnsupported>) noexcept must be not defined.");
}

TEST_F(DeserializeTests64, UInt16)
{
    rawBuf_[0] = 0b01000010_b;
    rawBuf_[1] = 0b11000011_b;
    std::uint16_t deserializedValue{};
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(breader, rabbit::make_bin_reader(rawBuf_));
            BOOST_LEAF_ASSIGN(deserializedValue,
                              rabbit::deserialize<std::uint16_t>(breader));
            return {};
        });
    ASSERT_EQ(deserializedValue, 0b0100001011000011);
}

TEST_F(DeserializeTests1, UInt16)
{
    expectError(reader_error::not_enough_buffer_size);
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(breader, rabbit::make_bin_reader(rawBuf_));
            BOOST_LEAF_CHECK(rabbit::deserialize<std::uint16_t>(breader));
            return {};
        });
}
