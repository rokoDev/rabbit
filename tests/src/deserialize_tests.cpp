#include <gtest/gtest.h>
#include <rabbit/deserialize.h>
#include <user_literals/user_literals.h>

#include "serialization_tests.h"

#ifdef BOOST_LEAF_NO_EXCEPTIONS

namespace boost
{
[[noreturn]] void throw_exception(std::exception const &e)
{
    std::cerr
        << "Terminating due to a C++ exception under BOOST_LEAF_NO_EXCEPTIONS: "
        << e.what();
    std::terminate();
}

struct source_location;
[[noreturn]] void throw_exception(std::exception const &e,
                                  boost::source_location const &)
{
    throw_exception(e);
}
}  // namespace boost

#endif

namespace rabbit
{
template <typename T>
using enable_if_uint16_t =
    std::enable_if_t<std::is_same_v<std::decay_t<T>, uint16_t>, T>;

template <typename T>
result<enable_if_uint16_t<T>> deserialize(reader &aReader, tag_t<T>) noexcept
{
    BOOST_LEAF_AUTO(r, aReader.getValue<T>());
    return r;
}
}  // namespace rabbit

using DeserializeTests64 = Data<uint8_t, 64>;
using DeserializeTests1 = Data<uint8_t, 1>;

TEST(DeserializeTests, StaticAsserts)
{
    static_assert(rabbit::is_deserializable_v<uint16_t>,
                  "uint16_t must be deserializable.");
    static_assert(rabbit::is_deserialize_defined_v<uint16_t>,
                  "result<uint16_t> deserialize(reader&, tag_t<uint16_t>) "
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
    rawBuf_[0] = 0b01000010;
    rawBuf_[1] = 0b11000011;
    uint16_t deserializedValue{};
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(reader, rabbit::make_bin_reader(rawBuf_));
            BOOST_LEAF_ASSIGN(deserializedValue,
                              rabbit::deserialize<uint16_t>(reader));
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
            BOOST_LEAF_AUTO(reader, rabbit::make_bin_reader(rawBuf_));
            BOOST_LEAF_CHECK(rabbit::deserialize<uint16_t>(reader));
            return {};
        });
}
