#include <gtest/gtest.h>
#include <rabbit/deserialize.h>
#include <rabbit/serialize.h>
#include <user_literals/user_literals.h>

#include <bitset>
#include <ostream>

#include "result_adapter_specs.h"
#include "serialization_tests.h"

// using writer = rabbit::simple_bin_writer<rabbit::core, rabbit::tag_t,
// rabbit::result_adapter<boost::leaf::result<void>>>; using reader =
// rabbit::simple_bin_reader<rabbit::core, rabbit::tag_t,
// rabbit::result_adapter<boost::leaf::result<void>>>;
using writer =
    rabbit::simple_bin_writer<rabbit::core, rabbit::tag_t,
                              rabbit::result_adapter<rabbit::writer_error>>;
using reader =
    rabbit::simple_bin_reader<rabbit::core, rabbit::tag_t,
                              rabbit::result_adapter<rabbit::reader_error>>;

std::ostream& operator<<(std::ostream& os, std::byte b)
{
    return os << std::bitset<8>(std::to_integer<unsigned int>(b));
}

using w_result_adapter = writer::result_adapter_t;
using r_result_adapter = reader::result_adapter_t;

TEST(SerializeTests, UInt16)
{
    std::byte rawData[16]{};
    writer w{rawData};
    const std::uint16_t value{1012_u16};
    ASSERT_TRUE(w_result_adapter::is_success(rabbit::serialize(w, value)));
    ASSERT_EQ(w.pos(), bit_pos(16));

    reader r{rawData};
    std::uint16_t deserialized_value{};
    ASSERT_TRUE(r_result_adapter::is_success(
        rabbit::deserialize(r, deserialized_value)));
    ASSERT_EQ(r.pos(), bit_pos(16));

    ASSERT_EQ(value, deserialized_value);
}

TEST(SerializeTests, Empty)
{
    std::byte rawData[16]{};
    writer w{rawData};
    Empty valueToSave;
    ASSERT_TRUE(
        w_result_adapter::is_success(rabbit::serialize(w, valueToSave)));
    ASSERT_EQ(w.pos(), bit_pos(0));
    ASSERT_EQ(rawData[0], 0b00000000_b);
    ASSERT_EQ(rawData[1], 0b00000000_b);
}

TEST(SerializeTests, NonEmpty)
{
    std::byte rawData[16]{};
    writer w{rawData};
    NotEmpty valueToSave{1_u16, 3_u16};
    ASSERT_TRUE(
        w_result_adapter::is_success(rabbit::serialize(w, valueToSave)));
    ASSERT_EQ(w.pos(), bit_pos(32));
    ASSERT_EQ(rawData[0], 0b00000000_b);
    ASSERT_EQ(rawData[1], 0b00000001_b);
    ASSERT_EQ(rawData[2], 0b00000000_b);
    ASSERT_EQ(rawData[3], 0b00000011_b);
    ASSERT_EQ(rawData[4], 0b00000000_b);
}

TEST(SerializeTests, Nested)
{
    std::byte rawData[16]{};
    writer w{rawData};
    Nested valueToSave{0b0000001001111001_u16,
                       {0b0000000000100000_u16, 0b0000001101101100_u16},
                       0b0000001000110001_u16};
    ASSERT_TRUE(
        w_result_adapter::is_success(rabbit::serialize(w, valueToSave)));
    ASSERT_EQ(w.pos(), bit_pos(64));
    ASSERT_EQ(rawData[0], 0b00000010_b);
    ASSERT_EQ(rawData[1], 0b01111001_b);
    ASSERT_EQ(rawData[2], 0b00000000_b);
    ASSERT_EQ(rawData[3], 0b00100000_b);
    ASSERT_EQ(rawData[4], 0b00000011_b);
    ASSERT_EQ(rawData[5], 0b01101100_b);
    ASSERT_EQ(rawData[6], 0b00000010_b);
    ASSERT_EQ(rawData[7], 0b00110001_b);
    ASSERT_EQ(rawData[8], 0b00000000_b);
}
