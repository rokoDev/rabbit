include(FetchContent)

# Setup directory to which dependencies will be downloaded
get_filename_component(deps_content "../deps_content"
                       REALPATH BASE_DIR "${CMAKE_BINARY_DIR}")
set(FETCHCONTENT_BASE_DIR ${deps_content})

# If dependencies are not available then download it and make available
FetchContent_Declare(
	fmt
	GIT_REPOSITORY https://github.com/fmtlib/fmt.git
	GIT_TAG        0cef1f819e9811209a6b110ae37fe65e70aa79b0
)

FetchContent_Declare(
	VersionHelper
    GIT_REPOSITORY https://github.com/rokoDev/VersionHelper.git
    GIT_TAG        cea7d1349d5f5167c045b90d12e91d2fefdc99b6
)

FetchContent_Declare(
	boost_pfr
	GIT_REPOSITORY https://github.com/boostorg/pfr.git
	GIT_TAG        8a8b5bc8d3ff673c4b278d145f6bf6973844d8e9
)

FetchContent_Declare(
	strong_type
	GIT_REPOSITORY https://github.com/rokoDev/strong_type.git
	GIT_TAG        065f7be922977a8e5a12277088974915446018a1
)

# If necessary it's possible to use VersionHelper from different location instead of downloaded from GitHub
# string(TOUPPER VersionHelper UP_VersionHelper)
# set(FETCHCONTENT_SOURCE_DIR_${UP_VersionHelper} ${CMAKE_CURRENT_LIST_DIR}/../../VersionHelper)

FetchContent_MakeAvailable(fmt VersionHelper boost_pfr strong_type)

# If use IDE add fmt target into deps source group
set_target_properties(fmt strong_type PROPERTIES FOLDER deps)

# Generate sources with information about rabbit version
list(APPEND CMAKE_MODULE_PATH "${versionhelper_SOURCE_DIR}")
include(VersionInfoUtils)
m_generate_version_info(PROJECT_NAME ${ProjectName}
	                    CPP_NAMESPACE "rabbit"
	                    BUILD_TYPES Debug Release
	                    IDE_SRC_GROUP "generated"
	                    TARGET_NAME "rabbit")