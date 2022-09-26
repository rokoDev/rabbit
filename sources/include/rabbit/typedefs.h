#ifndef rabbit_typedefs_h
#define rabbit_typedefs_h

#include <buffer/buffer.h>
#include <strong_type/strong_type.h>

#include <boost/leaf.hpp>
#include <boost/pfr.hpp>

namespace rabbit
{
namespace pfr = boost::pfr;
namespace leaf = boost::leaf;

template <class T>
using result = leaf::result<T>;

template <typename T>
struct tag_t
{
};

template <typename T>
constexpr tag_t<T> tag{};

template <typename StrongT>
struct NecessaryOps
    : strong::plus<StrongT>
    , strong::plus_assignment<StrongT>
    , strong::minus<StrongT>
    , strong::minus_assignment<StrongT>
    , strong::convertible_to_bool<StrongT>
    , strong::modulo<StrongT>
    , strong::comparisons<StrongT>
    , strong::implicitly_convertible_to_underlying<StrongT>
{
};

using SrcBitOffset =
    strong::strong_type<struct SrcBitOffsetTag, uint_fast8_t, NecessaryOps>;
using DstBitOffset =
    strong::strong_type<struct DstBitOffsetTag, uint_fast8_t, NecessaryOps>;
using BitOffset =
    strong::strong_type<struct BitOffsetTag, uint_fast8_t, NecessaryOps>;
using NumBits = buffer::n_bits;

template <typename StrongT>
struct DataPtrOps
    : strong::indirection<StrongT>
    , strong::subscription<StrongT>
    , strong::comparisons<StrongT>
    , strong::implicitly_convertible_to_underlying<StrongT>
{
};

using Src = strong::strong_type<struct SrcTag, uint8_t const *, DataPtrOps>;
using Dst = strong::strong_type<struct DstTag, uint8_t *, DataPtrOps>;

using buf_view = buffer::buffer_view<uint8_t>;
using buf_view_const = buffer::buffer_view_const<buf_view::value_type>;
using bit_pos = buffer::bit_pos;
using n_bytes = buffer::n_bytes;

class Core;

template <typename ImplT>
class bin_writer;

using writer = bin_writer<Core>;

template <typename ImplT>
class bin_reader;

using reader = bin_reader<Core>;
}  // namespace rabbit

#endif /* rabbit_typedefs_h */
