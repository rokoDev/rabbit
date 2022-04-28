include(FetchContent)

# Setup directory to which dependencies will be downloaded
get_filename_component(deps_content "../deps_content"
                       REALPATH BASE_DIR "${CMAKE_BINARY_DIR}")
set(FETCHCONTENT_BASE_DIR ${deps_content})

# If dependencies are not available then download it and make available
FetchContent_Declare(
	VersionHelper
    GIT_REPOSITORY https://github.com/rokoDev/VersionHelper.git
    GIT_TAG        b8f512ee783abdb44ebe3ea2fb59bec9f32b2e31
)

FetchContent_Declare(
	boost_pfr
	GIT_REPOSITORY https://github.com/boostorg/pfr.git
	GIT_TAG        8a8b5bc8d3ff673c4b278d145f6bf6973844d8e9
)

FetchContent_Declare(
	strong_type
	GIT_REPOSITORY https://github.com/rokoDev/strong_type.git
	GIT_TAG        a99212590fb052ad5ef2964c467ee71118fd7f5a
)

# If necessary it's possible to use VersionHelper from different location instead of downloaded from GitHub
# string(TOUPPER VersionHelper UP_VersionHelper)
# set(FETCHCONTENT_SOURCE_DIR_${UP_VersionHelper} ${CMAKE_CURRENT_LIST_DIR}/../../VersionHelper)

FetchContent_MakeAvailable(VersionHelper boost_pfr strong_type)
target_link_libraries(rabbit INTERFACE boost_pfr strong_type)

# If use IDE add strong_type target into deps source group
set_target_properties(strong_type PROPERTIES FOLDER deps)

# Generate sources with information about rabbit version
list(APPEND CMAKE_MODULE_PATH "${versionhelper_SOURCE_DIR}")
include(VersionInfoUtils)
m_generate_version_info(PROJECT_NAME ${ProjectName}
	                    CPP_NAMESPACE "rabbit"
	                    BUILD_TYPES Debug Release
	                    IDE_SRC_GROUP "generated"
	                    TARGET_NAME "rabbit"
                        HEADER_ONLY
	                    )