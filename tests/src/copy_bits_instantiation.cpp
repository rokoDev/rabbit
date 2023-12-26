#include <gtest/gtest.h>
#include <user_literals/user_literals.h>
#include <utils/utils.h>

#include <sstream>

#include "copy_bits_tests.h"

namespace test
{
using ::testing::Combine;
using ::testing::ValuesIn;

// copyBits tests instantiation
INSTANTIATE_TEST_SUITE_P(
    Rabbit, CopyBits5Args,
    Combine(ValuesIn(kDstData), ValuesIn(kSrcData), ValuesIn(kNBitsArray),
            ValuesIn(kDstOffsetsArray), ValuesIn(kSrcOffsetsArray)),
    [](const testing::TestParamInfo<CopyBits5Args::ParamType> &aInfo)
    {
        const auto kDstIndex = std::get<0>(aInfo.param).index;
        const auto kSrcIndex = std::get<1>(aInfo.param).index;
        const auto kNumBits = std::get<2>(aInfo.param);
        const auto kDstOffset = std::get<3>(aInfo.param);
        const auto kSrcOffset = std::get<4>(aInfo.param);

        std::string initStr;
        initStr.reserve(128);
        std::stringstream ss(initStr);
        ss << "Dst"sv << std::to_string(kDstIndex) << "Src"sv
           << std::to_string(kSrcIndex) << "kNBits"sv
           << std::to_string(kNumBits.get()) << "kDstOffset"sv
           << std::to_string(kDstOffset.get()) << "kSrcOffset"sv
           << std::to_string(kSrcOffset.get());
        return ss.str();
    });

INSTANTIATE_TEST_SUITE_P(
    Rabbit, CopyBits4Args,
    Combine(ValuesIn(kDstData), ValuesIn(kSrcData), ValuesIn(kNBitsArray),
            ValuesIn(kBitOffsetsArray)),
    [](const testing::TestParamInfo<CopyBits4Args::ParamType> &aInfo)
    {
        const auto kDstIndex = std::get<0>(aInfo.param).index;
        const auto kSrcIndex = std::get<1>(aInfo.param).index;
        const auto kNumBits = std::get<2>(aInfo.param);
        const auto kOffset = std::get<3>(aInfo.param);

        std::string initStr;
        initStr.reserve(128);
        std::stringstream ss(initStr);
        ss << "Dst"sv << std::to_string(kDstIndex) << "Src"sv
           << std::to_string(kSrcIndex) << "kNBits"sv
           << std::to_string(kNumBits.get()) << "kOffset"sv
           << std::to_string(kOffset.get());
        return ss.str();
    });

INSTANTIATE_TEST_SUITE_P(
    Rabbit, CopyBits3Args,
    Combine(ValuesIn(kDstData), ValuesIn(kSrcData), ValuesIn(kNBitsArray)),
    [](const testing::TestParamInfo<CopyBits3Args::ParamType> &aInfo)
    {
        const auto kDstIndex = std::get<0>(aInfo.param).index;
        const auto kSrcIndex = std::get<1>(aInfo.param).index;
        const auto kNumBits = std::get<2>(aInfo.param);

        std::string initStr;
        initStr.reserve(128);
        std::stringstream ss(initStr);
        ss << "Dst"sv << std::to_string(kDstIndex) << "Src"sv
           << std::to_string(kSrcIndex) << "kNBits"sv
           << std::to_string(kNumBits.get());
        return ss.str();
    });
}  // namespace test
