#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <user_literals/user_literals.h>

#include "serialization_tests.h"

using BuiltinTests = Data<uint8_t, 128>;

enum class eState : uint8_t
{
    kFirst = 0,
    kSecond,
    kThird,
    kFourth,
    kFifth,
    Count
};

struct Tricky
{
    bool a;
    uint16_t b;
    float c;
    int64_t d;
    eState e;
};

inline bool operator==(const Tricky& lhs, const Tricky& rhs)
{
    return (lhs.a == rhs.a) && (lhs.b == rhs.b) &&
           (utils::bit_cast<uint32_t>(lhs.c) ==
            utils::bit_cast<uint32_t>(rhs.c)) &&
           (lhs.d == rhs.d) && (lhs.e == rhs.e);
}

TEST_F(BuiltinTests, TrickySerialization)
{
    constexpr Tricky toSave{true, 1111_u16, 100.234f, -288247968372752384_i64,
                            eState::kFourth};
    Tricky restored{};
    bit_pos readerPos{};
    bit_pos writerPos{};
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(w, rabbit::make_bin_writer(rawBuf_));
            BOOST_LEAF_AUTO(r, rabbit::make_bin_reader(rawBuf_));
            BOOST_LEAF_CHECK(rabbit::serialize(w, toSave));
            BOOST_LEAF_ASSIGN(restored, rabbit::deserialize<Tricky>(r));
            readerPos = r.pos();
            writerPos = w.pos();
            return {};
        });
    ASSERT_EQ(readerPos, writerPos);
    ASSERT_EQ(readerPos, bit_pos(116));
    ASSERT_EQ(toSave, restored);
}
