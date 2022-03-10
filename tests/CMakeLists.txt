cmake_minimum_required(VERSION ${cmake_version})

set(ProjectName ${ProjectName}_tests)
project(${ProjectName})

get_filename_component(deps_content "../deps_content"
                       REALPATH BASE_DIR "${CMAKE_BINARY_DIR}")
set(FETCHCONTENT_BASE_DIR ${deps_content})

FetchContent_Declare(googletest
	GIT_REPOSITORY https://github.com/google/googletest.git
	GIT_TAG ae1b7ad4308249bfa928e65d1a33be117fc0992c)

FetchContent_MakeAvailable(googletest)

if (WIN32)
	option(CMAKE_USE_WIN32_THREADS_INIT "using WIN32 threads" ON)
	option(GTEST_DISABLE_PTHREADS "Disable uses of pthreads in gtest." ON)
  option(GTEST_FORCE_SHARED_CRT "Use shared (DLL) run-time lib even when Google Test is built as static lib." ON)
endif (WIN32)


macro(package_add_test TESTNAME)
  add_executable(${TESTNAME} "")
  target_sources(${TESTNAME} PRIVATE ${ARGN})
  target_link_libraries(${TESTNAME} gtest gmock gtest_main rabbit)
  add_test(NAME ${TESTNAME} COMMAND ${TESTNAME})
  set_target_properties(${TESTNAME} PROPERTIES FOLDER tests)

  # Create groups in the IDE which mirrors directory structure on the hard disk
  get_target_property(rabbit_tests_src ${TESTNAME} SOURCES)
  source_group(
    TREE   ${CMAKE_CURRENT_SOURCE_DIR}#/src
    FILES  ${rabbit_tests_src}
  )
endmacro()

package_add_test(${ProjectName}
	src/main_tests.cpp
	src/rabbit_tests.cpp
  src/bin_ops_tests.cpp
  src/endian_tests.cpp
	)

# If use IDE add gtest, gmock, gtest_main and gmock_main targets into deps/googletest group
set_target_properties(gtest gmock gtest_main gmock_main PROPERTIES FOLDER deps/googletest)