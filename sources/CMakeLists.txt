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
    include/rabbit/writer.h
    include/rabbit/reader.h
    include/rabbit/serialize.h
    include/rabbit/deserialize.h
    include/rabbit/typedefs.h
    include/rabbit/bit_ops.h
    include/rabbit/bit_holder.h
  )

set_property(TARGET rabbit APPEND PROPERTY SRC_DIRS "${CMAKE_CURRENT_LIST_DIR}/include")

include(GNUInstallDirs)
target_include_directories(rabbit INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}/include>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

get_target_property(PFR_INTERFACE_INC_DIR boost_pfr INTERFACE_INCLUDE_DIRECTORIES)
set_target_properties(boost_pfr PROPERTIES INTERFACE_INCLUDE_DIRECTORIES "")
target_include_directories(boost_pfr INTERFACE
  $<BUILD_INTERFACE:${PFR_INTERFACE_INC_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

# -------------------Installation-------------------
include(CMakePackageConfigHelpers)
write_basic_package_version_file(
  ${CMAKE_CURRENT_BINARY_DIR}/RabbitConfigVersion.cmake
  COMPATIBILITY SameMajorVersion
  ARCH_INDEPENDENT
)

install(
  TARGETS rabbit user_literals strong_type endian utils buffer boost_pfr interval rabbit_version sysops
  EXPORT rabbitTargets
  INCLUDES DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

install(
  EXPORT rabbitTargets
  FILE "RabbitConfig.cmake"
  NAMESPACE Rabbit::
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/rabbit
)

install(
  FILES ${CMAKE_CURRENT_BINARY_DIR}/RabbitConfigVersion.cmake
  DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/rabbit
)

install(
  DIRECTORY include
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)
