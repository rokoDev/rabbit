#include <gtest/gtest.h>
#include <rabbit/deserialize.h>
#include <user_literals/user_literals.h>

#include <boost/leaf.hpp>

#include "result_adapter_specs.h"
#include "serialization_tests.h"

using reader =
    rabbit::simple_bin_reader<rabbit::core, rabbit::tag_t, leaf_result_adapter>;

enum class eValidMinMax1
{
    kMin,
    kMax = kMin
};

enum class eValidMinMax2
{
    kMin = 1,
    kMax = 2
};

enum class eValidMinMax3
{
    kMin = 1,
    kValue1 = 3,
    kMax = 2
};

enum class eValidMinMax4
{
    kMin = -10,
    kValue1 = 3,
    kMax = -1
};

enum class eValidMinMax5
{
    kMax = 0,
    kValue1 = 3,
    kMin = -10,
};

enum class eInvalidMinMax1
{
};

enum class eInvalidMinMax2
{
    kMin
};

enum class eInvalidMinMax3
{
    kMax
};

enum class eInvalidMinMax4
{
    kMax
};

enum class eInvalidMinMax5
{
    kMin = 2,
    kMax = 1
};

enum class eInvalidMinMax6
{
    kMin = 2,
    kValue = 0,
    kMax = 1
};

using reader2 =
    rabbit::simple_bin_reader<rabbit::core, tag_t, leaf_result_adapter>;

decltype(auto) deserialize(reader2 &aReader, std::uint16_t &aValue,
                           tag_t<std::uint16_t>) noexcept
{
    aValue = aReader.template getValue<std::uint16_t>();
    return reader2::success();
}

using reader3 = rabbit::simple_bin_reader<rabbit::core, tag_t,
                                          rabbit::reader_error_result_adapter>;

constexpr decltype(auto) deserialize(reader3 &aReader, std::uint16_t &aValue,
                                     tag_t<std::uint16_t>) noexcept
{
    aValue = aReader.template getValue<std::uint16_t>();
    return reader3::success();
}

using DeserializeTests64 = Data<std::byte, 64>;
using DeserializeTests1 = Data<std::byte, 1>;

struct with_vec
{
    bool b;
    std::uint8_t u8;
    std::uint16_t u16;
    std::vector<int> v;
};

struct with_string
{
    bool b1;
    std::string s;
    std::uint8_t u8;
    bool b2;
};

struct with_vec_and_string
{
    bool b;
    std::vector<int> v;
    with_string ws;
};

struct with_vec_and_string2
{
    with_vec wv;
    with_string ws;
};

TEST(Validate, IsSizeDefinedByType)
{
    static_assert(rabbit::is_size_defined_by_type_v<Empty, rabbit::tag_t>);
    static_assert(rabbit::is_size_defined_by_type_v<NotEmpty, rabbit::tag_t>);
    static_assert(rabbit::is_size_defined_by_type_v<Nested, rabbit::tag_t>);
    static_assert(
        rabbit::is_size_defined_by_type_v<Unsupported, rabbit::tag_t>);
    static_assert(rabbit::is_size_defined_by_type_v<NestedWithUnsupported,
                                                    rabbit::tag_t>);
    static_assert(
        not rabbit::is_size_defined_by_type_v<with_vec, rabbit::tag_t>);
    static_assert(
        not rabbit::is_size_defined_by_type_v<with_string, rabbit::tag_t>);
    static_assert(not rabbit::is_size_defined_by_type_v<with_vec_and_string,
                                                        rabbit::tag_t>);
    static_assert(not rabbit::is_size_defined_by_type_v<with_vec_and_string2,
                                                        rabbit::tag_t>);
}

TEST(Validate, SizeDefinedByType)
{
    static_assert(rabbit::size_defined_by_type_v<Empty, rabbit::tag_t> == 0);
    static_assert(rabbit::size_defined_by_type_v<NotEmpty, rabbit::tag_t> ==
                  32);
    static_assert(rabbit::size_defined_by_type_v<Nested, rabbit::tag_t> == 64);
    static_assert(rabbit::size_defined_by_type_v<Unsupported, rabbit::tag_t> ==
                  56);
    static_assert(
        rabbit::size_defined_by_type_v<NestedWithUnsupported, rabbit::tag_t> ==
        120);
    static_assert(rabbit::size_defined_by_type_v<with_vec, rabbit::tag_t> ==
                  33);
    static_assert(rabbit::size_defined_by_type_v<with_string, rabbit::tag_t> ==
                  18);
    static_assert(
        rabbit::size_defined_by_type_v<with_vec_and_string, rabbit::tag_t> ==
        27);
    static_assert(
        rabbit::size_defined_by_type_v<with_vec_and_string2, rabbit::tag_t> ==
        51);
}

TEST(DeserializeTests, StaticAsserts)
{
    static_assert(rabbit::is_deserializable_v<std::uint16_t, reader>);
    static_assert(rabbit::is_deserialize_defined_v<std::uint16_t, reader>);

    static_assert(rabbit::is_deserializable_v<Empty, reader>);
    static_assert(not rabbit::is_deserialize_defined_v<Empty, reader>);

    static_assert(rabbit::is_deserializable_v<NotEmpty, reader>);
    static_assert(not rabbit::is_deserialize_defined_v<NotEmpty, reader>);

    static_assert(rabbit::is_deserializable_v<Nested, reader>);
    static_assert(not rabbit::is_deserialize_defined_v<Nested, reader>);

    static_assert(rabbit::is_deserializable_v<Unsupported, reader>);
    static_assert(not rabbit::is_deserialize_defined_v<Unsupported, reader>);

    static_assert(rabbit::is_deserializable_v<NestedWithUnsupported, reader>);
    static_assert(
        not rabbit::is_deserialize_defined_v<NestedWithUnsupported, reader>);
}

TEST(Validate, IsMinMaxEnum)
{
    static_assert(rabbit::is_min_max_enum_v<eValidMinMax1>);
    static_assert(rabbit::is_min_max_enum_v<eValidMinMax2>);
    static_assert(rabbit::is_min_max_enum_v<eValidMinMax3>);
    static_assert(rabbit::is_min_max_enum_v<eValidMinMax4>);
    static_assert(rabbit::is_min_max_enum_v<eValidMinMax5>);

    static_assert(not rabbit::is_min_max_enum_v<eInvalidMinMax1>);
    static_assert(not rabbit::is_min_max_enum_v<eInvalidMinMax2>);
    static_assert(not rabbit::is_min_max_enum_v<eInvalidMinMax3>);
    static_assert(not rabbit::is_min_max_enum_v<eInvalidMinMax4>);
    static_assert(not rabbit::is_min_max_enum_v<eInvalidMinMax5>);
    static_assert(not rabbit::is_min_max_enum_v<eInvalidMinMax6>);
    static_assert(not rabbit::is_min_max_enum_v<int>);
    static_assert(not rabbit::is_min_max_enum_v<double>);
    static_assert(not rabbit::is_min_max_enum_v<void>);
    static_assert(not rabbit::is_min_max_enum_v<float>);
}

TEST_F(DeserializeTests64, UInt16)
{
    rawBuf_[0] = 0b01000010_b;
    rawBuf_[1] = 0b11000011_b;
    std::uint16_t deserializedValue{};
    execute(
        [&]() -> decltype(auto)
        {
            auto breader = reader{rawBuf_};
            return rabbit::deserialize<std::uint16_t>(breader,
                                                      deserializedValue);
        });
    ASSERT_EQ(deserializedValue, 0b0100001011000011);
}

TEST_F(DeserializeTests1, UInt16)
{
    expectError(reader_error::run_out_of_data_source);
    execute(
        [&]() -> decltype(auto)
        {
            std::uint16_t value{};
            auto r = reader2{rawBuf_};
            static_assert(
                rabbit::is_size_defined_by_type_v<std::uint16_t, tag_t>);
            return rabbit::deserialize<std::uint16_t>(r, value);
        });
}

TEST_F(DeserializeTests1, UInt162)
{
    std::uint16_t value{};
    auto r = reader3{rawBuf_};
    static_assert(rabbit::is_size_defined_by_type_v<std::uint16_t, tag_t>);
    reader_error err = rabbit::deserialize<std::uint16_t>(r, value);
    ASSERT_EQ(err, reader_error::run_out_of_data_source);
}
