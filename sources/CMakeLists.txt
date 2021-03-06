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
  target_compile_definitions(rabbit INTERFACE HOST_BIG_ENDIAN)
endif()

target_compile_definitions(rabbit INTERFACE $<UPPER_CASE:$<CONFIG>>)
set_property(TARGET rabbit APPEND PROPERTY SRC_DIRS "${CMAKE_CURRENT_LIST_DIR}/include")
target_include_directories(rabbit INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)
