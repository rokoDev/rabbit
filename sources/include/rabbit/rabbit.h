#ifndef rabbit_rabbit_h
#define rabbit_rabbit_h

#include "deserialize.h"
#include "rabbit/version_info.h"
#include "serialize.h"

#ifdef BOOST_LEAF_NO_EXCEPTIONS

#include <iostream>

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
#endif /* BOOST_LEAF_NO_EXCEPTIONS */
#endif /* rabbit_rabbit_h */
