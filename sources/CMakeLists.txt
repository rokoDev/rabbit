list(APPEND UNIX_BUILD_TYPES_CLANG_FLAGS
  "-std=c++17"
  "-Werror"
  "-pedantic-errors"
  "-Wall"
  "-Wextra"
  "-Wconversion"
  "-Wsign-conversion"
  "-Wshadow-all"
  "-Wunreachable-code"
  "-Wuninitialized"
  "-Wold-style-cast"
  "-Wfloat-equal"
  "-Wcast-align"
  "-Winvalid-constexpr"
  "-fno-exceptions"
  "-fno-rtti"
  )

list(APPEND WIN_BUILD_TYPES_CLANG_FLAGS
  "/std:c++17"
  "/WX"
  "/permissive-"
  "/W4"
  #"/GX-" # Disable exception handling
  "/GR-" # Disable emission of RTTI data
  )

# Choose c++ compiler flags based on the compiler available or exit with error if no known compiler
if(CMAKE_CXX_COMPILER_ID STREQUAL Clang)
  if("${CMAKE_CXX_COMPILER_FRONTEND_VARIANT}" STREQUAL "MSVC")
    list(APPEND ALL_CXX_FLAGS "${WIN_BUILD_TYPES_CLANG_FLAGS}")
  elseif("${CMAKE_CXX_COMPILER_FRONTEND_VARIANT}" STREQUAL "GNU")
    list(APPEND ALL_CXX_FLAGS "${UNIX_BUILD_TYPES_CLANG_FLAGS}")
  else()
    message(FATAL_ERROR "CMAKE_CXX_COMPILER_FRONTEND_VARIANT: ${CMAKE_CXX_COMPILER_FRONTEND_VARIANT}(Unrecognized)")
  endif()
elseif(CMAKE_CXX_COMPILER_ID STREQUAL AppleClang)
  list(APPEND ALL_CXX_FLAGS "${UNIX_BUILD_TYPES_CLANG_FLAGS}")# "${CMAKE_CXX_FLAGS}"
elseif(CMAKE_CXX_COMPILER_ID STREQUAL GNU)
  list(APPEND ALL_CXX_FLAGS "${UNIX_BUILD_TYPES_CLANG_FLAGS}")
else()
  message(FATAL_ERROR "CMAKE_CXX_COMPILER_ID: ${CMAKE_CXX_COMPILER_ID}(Unrecognized)")
endif()

message(STATUS "ALL_CXX_FLAGS: ${ALL_CXX_FLAGS}")

if(WIN32)
  # turn off RTTI and Exceptions (including in STL)
  string(REGEX REPLACE "/GR" "/GR-" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
  string(REGEX REPLACE "/EHsc" "/EHs-c-" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
  add_definitions(-D_HAS_EXCEPTIONS=0) # for STL
endif()

target_sources(rabbit
  PRIVATE
    include/rabbit/rabbit.h
    include/rabbit/endian.h
    include/rabbit/bin_ops.h
    include/rabbit/utils.h
    include/rabbit/details.h
    include/rabbit/user_literals.h
  )

include(TestBigEndian)
test_big_endian(HOST_BIG_ENDIAN)
if(HOST_BIG_ENDIAN)
  target_compile_definitions(rabbit PRIVATE HOST_BIG_ENDIAN)
endif()

target_compile_definitions(rabbit INTERFACE $<UPPER_CASE:$<CONFIG>>)
set_property(TARGET rabbit APPEND PROPERTY SRC_DIRS "${CMAKE_CURRENT_LIST_DIR}/include")
target_link_libraries(rabbit INTERFACE boost_pfr strong_type)
target_include_directories(rabbit INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)