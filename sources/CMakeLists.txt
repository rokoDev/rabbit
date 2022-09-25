if(WIN32)
  # turn off RTTI and Exceptions (including in STL)
  string(REGEX REPLACE "/GR" "/GR-" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
  string(REGEX REPLACE "/EHsc" "/EHs-c-" CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS}")
  add_definitions(-D_HAS_EXCEPTIONS=0) # for STL
endif()

target_sources(rabbit
  PRIVATE
    include/rabbit/rabbit.h
    include/rabbit/bin_ops.h
    include/rabbit/details.h
    include/rabbit/bin_writer.h
    include/rabbit/bin_reader.h
  )

target_compile_definitions(rabbit
  INTERFACE
  $<UPPER_CASE:$<CONFIG>>
  BOOST_LEAF_NO_EXCEPTIONS=0
  BOOST_LEAF_CFG_STD_SYSTEM_ERROR=0
  BOOST_LEAF_CFG_STD_STRING=0
  BOOST_LEAF_CFG_DIAGNOSTICS=0
  BOOST_LEAF_CFG_GNUC_STMTEXPR=0
  )

set_property(TARGET rabbit APPEND PROPERTY SRC_DIRS "${CMAKE_CURRENT_LIST_DIR}/include")
target_include_directories(rabbit INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:include>
)
