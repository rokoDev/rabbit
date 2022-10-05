#include <gmock/gmock.h>
#include <gtest/gtest.h>
#include <user_literals/user_literals.h>

#include <cstring>
#include <valarray>
#include <vector>

#include "serialization_tests.h"

using BuiltinTests = Data<uint8_t, 128>;

enum class eState : uint8_t
{
    kFirst = 0,
    kSecond,
    kThird,
    kFourth,
    kFifth
};
RABBIT_ENUM_MIN_MAX(eState, kFirst, kFifth)

enum class eCustom : int
{
    kFirst = -3,
    kSecond,
    kThird,
    kFourth,
    kFifth
};
RABBIT_ENUM_MIN_MAX(eCustom, kFirst, kFifth)

struct Tricky
{
    bool a;
    uint16_t b;
    float c;
    eCustom f;
    int64_t d;
    eState e;
};

inline bool operator==(const Tricky& lhs, const Tricky& rhs)
{
    return (lhs.a == rhs.a) && (lhs.b == rhs.b) &&
           (utils::bit_cast<uint32_t>(lhs.c) ==
            utils::bit_cast<uint32_t>(rhs.c)) &&
           (lhs.f == rhs.f) && (lhs.d == rhs.d) && (lhs.e == rhs.e);
}

struct WithVectorAndString
{
    int32_t a;
    std::vector<int8_t> b;
    float c;
    bool d;
    std::string s;
};

inline bool operator==(const WithVectorAndString& lhs,
                       const WithVectorAndString& rhs)
{
    return (lhs.a == rhs.a) && (lhs.b.size() == rhs.b.size()) &&
           (!std::memcmp(lhs.b.data(), rhs.b.data(), lhs.b.size())) &&
           (utils::bit_cast<uint32_t>(lhs.c) ==
            utils::bit_cast<uint32_t>(rhs.c)) &&
           (lhs.d == rhs.d) && (lhs.s == rhs.s);
}

TEST_F(BuiltinTests, TrickySerialization)
{
    constexpr Tricky toSave{true,
                            1111_u16,
                            100.234f,
                            eCustom::kSecond,
                            -288247968372752384_i64,
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
    ASSERT_EQ(readerPos, bit_pos(119));
    ASSERT_EQ(toSave, restored);
}

TEST_F(BuiltinTests, VectorSerialization)
{
    using VecT = std::vector<int16_t>;
    const VecT vec{-101, 24, 3};
    VecT restored{};
    bit_pos readerPos{};
    bit_pos writerPos{};
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(w, rabbit::make_bin_writer(rawBuf_));
            BOOST_LEAF_AUTO(r, rabbit::make_bin_reader(rawBuf_));
            BOOST_LEAF_CHECK(rabbit::serialize(w, vec));
            BOOST_LEAF_ASSIGN(restored, rabbit::deserialize<VecT>(r));
            readerPos = r.pos();
            writerPos = w.pos();
            return {};
        });

    ASSERT_EQ(readerPos, writerPos);
    ASSERT_EQ(readerPos, bit_pos(80));

    for (std::size_t i = 0; i < vec.size(); ++i)
    {
        ASSERT_EQ(vec[i], restored[i]);
    }
}

TEST_F(BuiltinTests, EmptyVectorSerialization)
{
    using VecT = std::vector<int16_t>;
    const VecT vec{};
    VecT restored{};
    bit_pos readerPos{};
    bit_pos writerPos{};
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(w, rabbit::make_bin_writer(rawBuf_));
            BOOST_LEAF_AUTO(r, rabbit::make_bin_reader(rawBuf_));
            BOOST_LEAF_CHECK(rabbit::serialize(w, vec));
            BOOST_LEAF_ASSIGN(restored, rabbit::deserialize<VecT>(r));
            readerPos = r.pos();
            writerPos = w.pos();
            return {};
        });

    ASSERT_EQ(readerPos, writerPos);
    ASSERT_EQ(readerPos, bit_pos(1));
    ASSERT_EQ(vec.size(), restored.size());
    ASSERT_EQ(vec.size(), 0_uz);
}

TEST_F(BuiltinTests, VectorOfChars)
{
    using VecT = std::vector<char>;
    constexpr std::size_t kSize = 100_uz;
    VecT vec;
    vec.reserve(kSize);

    char value = static_cast<char>(-50);
    for (std::size_t i = 0_uz; i < kSize; ++i)
    {
        vec.push_back(value++);
    }

    VecT restored{};
    bit_pos readerPos{};
    bit_pos writerPos{};
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(w, rabbit::make_bin_writer(rawBuf_));
            BOOST_LEAF_AUTO(r, rabbit::make_bin_reader(rawBuf_));
            BOOST_LEAF_CHECK(rabbit::serialize(w, vec));
            BOOST_LEAF_ASSIGN(restored, rabbit::deserialize<VecT>(r));
            readerPos = r.pos();
            writerPos = w.pos();
            return {};
        });

    ASSERT_EQ(readerPos, writerPos);
    ASSERT_EQ(readerPos, bit_pos((kSize + sizeof(uint32_t)) * CHAR_BIT));
    ASSERT_EQ(vec.size(), restored.size());
    ASSERT_EQ(vec.size(), kSize);
    for (std::size_t i = 0_uz; i < kSize; ++i)
    {
        ASSERT_EQ(restored[i], vec[i]);
    }
}

TEST_F(BuiltinTests, StructWithVector)
{
    const WithVectorAndString toSave{
        1111_i32, {-120, 20, 5}, 100.234f, true, std::string("some string")};
    WithVectorAndString restored{};
    bit_pos readerPos{};
    bit_pos writerPos{};
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(w, rabbit::make_bin_writer(rawBuf_));
            BOOST_LEAF_AUTO(r, rabbit::make_bin_reader(rawBuf_));
            BOOST_LEAF_CHECK(rabbit::serialize(w, toSave));
            BOOST_LEAF_ASSIGN(restored,
                              rabbit::deserialize<WithVectorAndString>(r));
            readerPos = r.pos();
            writerPos = w.pos();
            return {};
        });
    ASSERT_EQ(readerPos, writerPos);
    const bit_pos expectedBitPos(
        (sizeof(int32_t) + sizeof(uint32_t) +
         sizeof(decltype(toSave.b)::value_type) * toSave.b.size() +
         sizeof(float) + sizeof(uint32_t) +
         sizeof(decltype(toSave.s)::value_type) * toSave.s.size()) *
            CHAR_BIT +
        1);
    ASSERT_EQ(readerPos, expectedBitPos);
    ASSERT_EQ(restored, toSave);
}

TEST_F(BuiltinTests, ValarrayOfChars)
{
    constexpr std::size_t kSize = 100_uz;
    using VecT = std::valarray<char>;
    VecT vec(kSize);

    char value = static_cast<char>(-50);
    for (std::size_t i = 0_uz; i < kSize; ++i)
    {
        vec[i] = value++;
    }

    VecT restored{};
    bit_pos readerPos{};
    bit_pos writerPos{};
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(w, rabbit::make_bin_writer(rawBuf_));
            BOOST_LEAF_AUTO(r, rabbit::make_bin_reader(rawBuf_));
            BOOST_LEAF_CHECK(rabbit::serialize(w, vec));
            BOOST_LEAF_ASSIGN(restored, rabbit::deserialize<VecT>(r));
            readerPos = r.pos();
            writerPos = w.pos();
            return {};
        });

    ASSERT_EQ(readerPos, writerPos);
    ASSERT_EQ(readerPos, bit_pos((kSize + sizeof(uint32_t)) * CHAR_BIT));
    ASSERT_EQ(vec.size(), restored.size());
    ASSERT_EQ(vec.size(), kSize);
    for (std::size_t i = 0_uz; i < kSize; ++i)
    {
        ASSERT_EQ(restored[i], vec[i]);
    }
}

TEST_F(BuiltinTests, EmptyValarray)
{
    using VecT = std::valarray<int16_t>;
    const VecT vec{};
    VecT restored{};
    bit_pos readerPos{};
    bit_pos writerPos{};
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(w, rabbit::make_bin_writer(rawBuf_));
            BOOST_LEAF_AUTO(r, rabbit::make_bin_reader(rawBuf_));
            BOOST_LEAF_CHECK(rabbit::serialize(w, vec));
            BOOST_LEAF_ASSIGN(restored, rabbit::deserialize<VecT>(r));
            readerPos = r.pos();
            writerPos = w.pos();
            return {};
        });

    ASSERT_EQ(readerPos, writerPos);
    ASSERT_EQ(readerPos, bit_pos(1));
    ASSERT_EQ(vec.size(), restored.size());
    ASSERT_EQ(vec.size(), 0_uz);
}

TEST_F(BuiltinTests, ValarrayOfInt32)
{
    constexpr std::size_t kSize = 10_uz;
    using DataT = int32_t;
    using VecT = std::valarray<DataT>;
    VecT vec(kSize);

    DataT value = static_cast<DataT>(-50);
    for (std::size_t i = 0_uz; i < kSize; ++i)
    {
        vec[i] = value++;
    }

    VecT restored{};
    bit_pos readerPos{};
    bit_pos writerPos{};
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(w, rabbit::make_bin_writer(rawBuf_));
            BOOST_LEAF_AUTO(r, rabbit::make_bin_reader(rawBuf_));
            BOOST_LEAF_CHECK(rabbit::serialize(w, vec));
            BOOST_LEAF_ASSIGN(restored, rabbit::deserialize<VecT>(r));
            readerPos = r.pos();
            writerPos = w.pos();
            return {};
        });

    ASSERT_EQ(readerPos, writerPos);
    ASSERT_EQ(readerPos,
              bit_pos((sizeof(DataT) * kSize + sizeof(uint32_t)) * CHAR_BIT));
    ASSERT_EQ(vec.size(), restored.size());
    ASSERT_EQ(vec.size(), kSize);
    for (std::size_t i = 0_uz; i < kSize; ++i)
    {
        ASSERT_EQ(restored[i], vec[i]);
    }
}

TEST_F(BuiltinTests, EmptyStdString)
{
    const std::string str;
    std::string restored{};
    bit_pos readerPos{};
    bit_pos writerPos{};
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(w, rabbit::make_bin_writer(rawBuf_));
            BOOST_LEAF_AUTO(r, rabbit::make_bin_reader(rawBuf_));
            BOOST_LEAF_CHECK(rabbit::serialize(w, str));
            BOOST_LEAF_ASSIGN(restored, rabbit::deserialize<std::string>(r));
            readerPos = r.pos();
            writerPos = w.pos();
            return {};
        });

    ASSERT_EQ(readerPos, writerPos);
    ASSERT_EQ(readerPos, bit_pos(1));
    ASSERT_EQ(str.size(), restored.size());
    ASSERT_EQ(str.size(), 0_uz);
}

TEST_F(BuiltinTests, NotEmptyStdString)
{
    const std::string str("Hi! This is test string.");
    const std::size_t kSize = str.size();
    std::string restored{};
    bit_pos readerPos{};
    bit_pos writerPos{};
    execute(
        [&]() -> result<void>
        {
            BOOST_LEAF_AUTO(w, rabbit::make_bin_writer(rawBuf_));
            BOOST_LEAF_AUTO(r, rabbit::make_bin_reader(rawBuf_));
            BOOST_LEAF_CHECK(rabbit::serialize(w, str));
            BOOST_LEAF_ASSIGN(restored, rabbit::deserialize<std::string>(r));
            readerPos = r.pos();
            writerPos = w.pos();
            return {};
        });

    ASSERT_EQ(readerPos, writerPos);
    ASSERT_EQ(readerPos, bit_pos((kSize + sizeof(uint32_t)) * CHAR_BIT));
    ASSERT_EQ(str.size(), restored.size());
    ASSERT_EQ(str.size(), kSize);
    ASSERT_EQ(restored, str);
}

TEST_F(BuiltinTests, IsCompileComputableSize)
{
    static_assert(
        not rabbit::is_compile_time_computable_size_v<std::vector<char>>,
        "serialized size of 'std::vector<char>' must be not compile "
        "time computable.");
    static_assert(
        not rabbit::is_compile_time_computable_size_v<std::valarray<char>>,
        "serialized size of 'std::valarray<char>' must be not "
        "compile time computable.");

    static_assert(rabbit::is_compile_time_computable_size_v<bool>,
                  "serialized size of 'bool' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<bool> == 1_uz,
                  "invalid bit size of bool.");

    static_assert(
        rabbit::is_compile_time_computable_size_v<int8_t>,
        "serialized size of 'int8_t' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<int8_t> == 8_uz,
                  "invalid bit size of int8_t.");
    static_assert(
        rabbit::is_compile_time_computable_size_v<int16_t>,
        "serialized size of 'int16_t' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<int16_t> == 16_uz,
                  "invalid bit size of int16_t.");
    static_assert(
        rabbit::is_compile_time_computable_size_v<int32_t>,
        "serialized size of 'int32_t' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<int32_t> == 32_uz,
                  "invalid bit size of int32_t.");
    static_assert(
        rabbit::is_compile_time_computable_size_v<int64_t>,
        "serialized size of 'int64_t' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<int64_t> == 64_uz,
                  "invalid bit size of int64_t.");

    static_assert(
        rabbit::is_compile_time_computable_size_v<uint8_t>,
        "serialized size of 'uint16_t' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<uint8_t> == 8_uz,
                  "invalid bit size of uint8_t.");
    static_assert(
        rabbit::is_compile_time_computable_size_v<uint16_t>,
        "serialized size of 'uint16_t' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<uint16_t> == 16_uz,
                  "invalid bit size of uint16_t.");
    static_assert(
        rabbit::is_compile_time_computable_size_v<uint32_t>,
        "serialized size of 'uint32_t' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<uint32_t> == 32_uz,
                  "invalid bit size of uint32_t.");
    static_assert(
        rabbit::is_compile_time_computable_size_v<uint64_t>,
        "serialized size of 'uint64_t' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<uint64_t> == 64_uz,
                  "invalid bit size of uint64_t.");

    static_assert(
        rabbit::is_compile_time_computable_size_v<float>,
        "serialized size of 'float' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<float> == 32_uz,
                  "invalid bit size of float.");
    static_assert(
        rabbit::is_compile_time_computable_size_v<double>,
        "serialized size of 'double' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<double> == 64_uz,
                  "invalid bit size of double.");

    static_assert(
        rabbit::is_compile_time_computable_size_v<eState>,
        "serialized size of 'eState' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<eState> == 3_uz,
                  "invalid bit size of eState.");
    static_assert(
        rabbit::is_compile_time_computable_size_v<eCustom>,
        "serialized size of 'eCustom' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<eCustom> == 3_uz,
                  "invalid bit size of eCustom.");

    static_assert(
        rabbit::is_compile_time_computable_size_v<Tricky>,
        "serialized size of 'Tricky' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<Tricky> == 119_uz,
                  "invalid bit size of Tricky.");

    static_assert(
        rabbit::is_compile_time_computable_size_v<Nested>,
        "serialized size of 'Nested' must be compile time computable.");
    static_assert(rabbit::bit_sizeof_v<Nested> == 64_uz,
                  "invalid bit size of Nested.");

    static_assert(
        not rabbit::is_compile_time_computable_size_v<WithVectorAndString>,
        "size of structure with vector should be not compile time computable.");

    static_assert(
        not rabbit::is_compile_time_computable_size_v<std::valarray<char>>,
        "size of std::valarray should be not compile time computable.");
}

TEST(Builtin, TestSize)
{
    [[maybe_unused]] const WithVectorAndString toSave{
        1111_i32, {-120, 20, 5}, 100.234f, true, std::string("just a string")};

    const std::size_t kNumBits = rabbit::bit_sizeof(toSave);
    ASSERT_EQ(kNumBits, 257_uz);

    const std::size_t kNumBytes = rabbit::byte_sizeof(toSave);
    ASSERT_EQ(kNumBytes, 33_uz);
}
