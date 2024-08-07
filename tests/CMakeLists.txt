cmake_minimum_required(VERSION ${cmake_version})

set(ProjectName ${ProjectName}_tests)
project(${ProjectName})

FetchContent_Declare(
  googletest
  GIT_REPOSITORY https://github.com/google/googletest.git
  GIT_TAG        9d43b27f7a873596496a2ea70721b3f9eb82df01
  )

FetchContent_MakeAvailable(googletest)



add_library(tests_main STATIC)
target_sources(tests_main
  PRIVATE
  src/main_tests.cpp
  )
target_link_libraries(tests_main PUBLIC gtest)
get_target_property(tests_main_sources tests_main SOURCES)
source_group(
  TREE   ${CMAKE_CURRENT_SOURCE_DIR}/src
  FILES  ${tests_main_sources}
)

if (WIN32)
  option(CMAKE_USE_WIN32_THREADS_INIT "using WIN32 threads" ON)
  option(GTEST_DISABLE_PTHREADS "Disable uses of pthreads in gtest." ON)
  option(GTEST_FORCE_SHARED_CRT "Use shared (DLL) run-time lib even when Google Test is built as static lib." ON)
endif (WIN32)

function(package_add_test)
  set(prefix ARG)
  set(noValues)
  set(singleValues TEST_TARGET_NAME)
  set(multiValues
    TEST_SOURCES
    EXTRA_TARGETS
    DEFS
    )

  cmake_parse_arguments(${prefix}
                        "${noValues}"
                        "${singleValues}"
                        "${multiValues}"
                        ${ARGN})

  foreach(arg IN LISTS noValues singleValues multiValues)
      set(${arg} ${${prefix}_${arg}})
  endforeach()

  add_executable(${TEST_TARGET_NAME})
  target_sources(${TEST_TARGET_NAME} PRIVATE ${TEST_SOURCES})
  target_include_directories(${TEST_TARGET_NAME} PUBLIC include)
  foreach(target_to_link IN LISTS EXTRA_TARGETS)
      target_link_libraries(${TEST_TARGET_NAME} PUBLIC ${target_to_link})
  endforeach()

  foreach(define IN LISTS DEFS)
      target_compile_definitions(${TEST_TARGET_NAME} PRIVATE ${define})
  endforeach()

  add_test(NAME ${TEST_TARGET_NAME} COMMAND ${TEST_TARGET_NAME})

  # Create groups in the IDE which mirrors directory structure on the hard disk
  get_target_property(rabbit_tests_src ${TEST_TARGET_NAME} SOURCES)
  source_group(
    TREE   ${CMAKE_CURRENT_SOURCE_DIR}#/src
    FILES  ${rabbit_tests_src}
  )

  # Place all tests targets under "test" source group in IDE
  set_target_properties(${TEST_TARGET_NAME} PROPERTIES FOLDER tests)

  # If colours in googletest are not disabled then in some versions of commandline tools which is used in Xcode 13.3 this
  # can lead to output with escape sequences. Because of that its better to disable colours in googletest.
  set_target_properties(${TEST_TARGET_NAME} PROPERTIES XCODE_GENERATE_SCHEME ON)
  set_target_properties(${TEST_TARGET_NAME} PROPERTIES XCODE_SCHEME_ARGUMENTS "--gtest_color=no")
endfunction()

# Build test_helpers object library only once to use it in few places later
add_library(test_helpers OBJECT)
target_sources(test_helpers PRIVATE
  include/test_helpers.h
  src/test_helpers.cpp
  )
target_include_directories(test_helpers PUBLIC include)
target_link_libraries(test_helpers PUBLIC rabbit)

get_target_property(test_helpers_src test_helpers SOURCES)
  source_group(
    TREE   ${CMAKE_CURRENT_SOURCE_DIR}
    FILES  ${test_helpers_src}
  )
# Place all test_helpers target under "test" source group in IDE
set_target_properties(test_helpers PROPERTIES FOLDER tests)


set(test_src
  src/test_helpers_tests.cpp
  )
package_add_test(
  TEST_TARGET_NAME test_helpers_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS test_helpers tests_main
  )

set(test_src
  include/copy_bits_constexpr_tests.h
  src/copy_bits_constexpr_tests.cpp
  src/copy_bits_constexpr_instantiation.cpp
  )
package_add_test(
  TEST_TARGET_NAME copy_bits_v1_constexpr_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS test_helpers tests_main
  DEFS CORE_V1
  )

set(test_src
  include/copy_bits_constexpr_tests.h
  src/copy_bits_constexpr_tests.cpp
  src/copy_bits_constexpr_instantiation.cpp
  )
package_add_test(
  TEST_TARGET_NAME copy_bits_v2_constexpr_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS test_helpers tests_main
  DEFS CORE_V2
  )

set(test_src
  include/copy_bits_tests.h
  src/copy_bits_tests.cpp
  src/copy_bits_instantiation.cpp
  )
package_add_test(
  TEST_TARGET_NAME copy_bits_v1_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS test_helpers tests_main
  DEFS CORE_V1
  )

set(test_src
  include/copy_bits_tests.h
  src/copy_bits_tests.cpp
  src/copy_bits_instantiation.cpp
  )
package_add_test(
  TEST_TARGET_NAME copy_bits_v2_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS test_helpers tests_main
  DEFS CORE_V2
  )

set(test_src
  include/add_value_tests.h
  src/add_value_tests.cpp
  src/add_value_instantiation.cpp
  )
package_add_test(
  TEST_TARGET_NAME add_value_v1_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS test_helpers tests_main
  DEFS CORE_V1
  )

set(test_src
  include/add_value_constexpr_tests.h
  src/add_value_constexpr_tests.cpp
  src/add_value_constexpr_instantiation.cpp
  )
package_add_test(
  TEST_TARGET_NAME add_value_v1_constexpr_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS test_helpers tests_main
  DEFS CORE_V1
  )

set(test_src
  include/add_value_tests.h
  src/add_value_tests.cpp
  src/add_value_instantiation.cpp
  )
package_add_test(
  TEST_TARGET_NAME add_value_v2_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS test_helpers tests_main
  DEFS CORE_V2
  )

set(test_src
  include/add_value_constexpr_tests.h
  src/add_value_constexpr_tests.cpp
  src/add_value_constexpr_instantiation.cpp
  )
package_add_test(
  TEST_TARGET_NAME add_value_v2_constexpr_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS test_helpers tests_main
  DEFS CORE_V2
  )

set(test_src
  include/get_value_tests.h
  src/get_value_tests.cpp
  src/get_value_instantiation.cpp
  )
package_add_test(
  TEST_TARGET_NAME get_value_v1_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS test_helpers tests_main
  DEFS CORE_V1
  )

set(test_src
  include/get_value_constexpr_tests.h
  src/get_value_constexpr_tests.cpp
  src/get_value_constexpr_instantiation.cpp
  )
package_add_test(
  TEST_TARGET_NAME get_value_v1_constexpr_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS test_helpers tests_main
  DEFS CORE_V1
  )

set(test_src
  include/get_value_tests.h
  src/get_value_tests.cpp
  src/get_value_instantiation.cpp
  )
package_add_test(
  TEST_TARGET_NAME get_value_v2_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS test_helpers tests_main
  DEFS CORE_V2
  )

set(test_src
  include/get_value_constexpr_tests.h
  src/get_value_constexpr_tests.cpp
  src/get_value_constexpr_instantiation.cpp
  )
package_add_test(
  TEST_TARGET_NAME get_value_v2_constexpr_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS test_helpers tests_main
  DEFS CORE_V2
  )

set(test_src
  include/buffer_tests.h
  src/bin_ops_tests.cpp
  )
package_add_test(
  TEST_TARGET_NAME misc_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS rabbit tests_main
	)

set(test_src
  include/serialization_tests.h
  src/writer_tests.cpp
  )
package_add_test(
  TEST_TARGET_NAME writer_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS rabbit tests_main gmock
  )

set(test_src
  include/serialization_tests.h
  src/reader_tests.cpp
  )
package_add_test(
  TEST_TARGET_NAME reader_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS rabbit tests_main gmock
  )

set(test_src
  include/serialization_tests.h
  include/result_adapter_specs.h
  src/serialize_tests.cpp
  )
package_add_test(
  TEST_TARGET_NAME serialize_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS rabbit tests_main gmock
  )

set(test_src
  include/serialization_tests.h
  include/result_adapter_specs.h
  src/deserialize_tests.cpp
  )
package_add_test(
  TEST_TARGET_NAME deserialize_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS rabbit tests_main gmock
  )

set(test_src
  src/v2_details_tests.cpp
  )
package_add_test(
  TEST_TARGET_NAME v2_details_tests
  TEST_SOURCES ${test_src}
  EXTRA_TARGETS rabbit tests_main gmock test_helpers
  )

# If use IDE add gtest, gmock, gtest_main and gmock_main targets into deps/googletest group
set_target_properties(gtest gmock gtest_main gmock_main PROPERTIES FOLDER deps/googletest)
