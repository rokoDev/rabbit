include(FetchContent)

# Setup directory to which dependencies will be downloaded
cmake_path(APPEND FETCHCONTENT_BASE_DIR "${CMAKE_SOURCE_DIR}" "deps_content" "${CMAKE_GENERATOR_NAME_WITHOUT_SPACES}")

# If dependencies are not available then download it and make available
FetchContent_Declare(
	VersionHelper
    GIT_REPOSITORY https://github.com/rokoDev/VersionHelper.git
    GIT_TAG        497e036e8ce2879aa193d905999b535da4aff8f8
)

FetchContent_Declare(
	boost_pfr
	GIT_REPOSITORY https://github.com/boostorg/pfr.git
	GIT_TAG        8a8b5bc8d3ff673c4b278d145f6bf6973844d8e9
)

FetchContent_Declare(
	strong_type
	GIT_REPOSITORY https://github.com/rokoDev/strong_type.git
	GIT_TAG        1b3c655287d7e05a1d37497facee8612c4109c00
)

FetchContent_Declare(
	user_literals
	GIT_REPOSITORY https://github.com/rokoDev/user_literals.git
	GIT_TAG        5c62773ff93b21cc5868eb3c7377deb9a8d8ec97
)

FetchContent_Declare(
	endian
	GIT_REPOSITORY https://github.com/rokoDev/endian.git
	GIT_TAG        3426169e75f102a78b60dbc767e06d116ca76b26
)

# If necessary it's possible to use VersionHelper from different location instead of downloaded from GitHub
# string(TOUPPER VersionHelper UP_VersionHelper)
# set(FETCHCONTENT_SOURCE_DIR_${UP_VersionHelper} ${CMAKE_SOURCE_DIR}/../VersionHelper)

FetchContent_MakeAvailable(VersionHelper boost_pfr strong_type user_literals endian)
target_link_libraries(rabbit INTERFACE boost_pfr strong_type user_literals endian)

# If use IDE add strong_type target into deps source group
set_target_properties(strong_type user_literals endian PROPERTIES FOLDER deps)

# Generate sources with information about rabbit version
list(APPEND CMAKE_MODULE_PATH "${versionhelper_SOURCE_DIR}")
include(VersionInfoUtils)
m_generate_version_info(PROJECT_NAME ${ProjectName}
	                    CPP_NAMESPACE "rabbit"
	                    BUILD_TYPES ${SUPPORTED_CONFIGURATIONS}
	                    IDE_SRC_GROUP "generated"
	                    TARGET_NAME "rabbit"
                        HEADER_ONLY
	                    )
