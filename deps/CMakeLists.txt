include(FetchContent)

# Setup directory to which dependencies will be downloaded
cmake_path(APPEND FETCHCONTENT_BASE_DIR "${CMAKE_SOURCE_DIR}/../" "deps_content" "${CMAKE_GENERATOR_NAME_WITHOUT_SPACES}")

# If dependencies are not available then download it and make available
FetchContent_Declare(
	VersionHelper
    GIT_REPOSITORY https://github.com/rokoDev/VersionHelper.git
    GIT_TAG        497e036e8ce2879aa193d905999b535da4aff8f8
)

FetchContent_Declare(
	strong_type
	GIT_REPOSITORY https://github.com/rokoDev/strong_type.git
	GIT_TAG        b34f11ad2cbb164030a56a80e488cfd716d43b5d
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

FetchContent_Declare(
	utils
	GIT_REPOSITORY https://github.com/rokoDev/utils.git
	GIT_TAG        394c6168272008e71af65f2a04eb87597314e6d0
)

FetchContent_Declare(
	buffer
	GIT_REPOSITORY https://github.com/rokoDev/buffer.git
	GIT_TAG        dbd42a85f1522ed58ac074702b69b6eb99f5f18e
)

FetchContent_Declare(
	boost_leaf
	GIT_REPOSITORY https://github.com/boostorg/leaf.git
	GIT_TAG        f2ec744ce9ca1934193b85f7094c1931177a451c
)

FetchContent_Declare(
	boost_pfr
	GIT_REPOSITORY https://github.com/boostorg/pfr.git
	GIT_TAG        58dcb40a4906cb0f5f7b162599e58a018ca8f040
)

FetchContent_Declare(
	interval
	GIT_REPOSITORY https://github.com/rokoDev/interval.git
	GIT_TAG        f1ef357b5b41ade09de06b4faacd5aa6287dd1d7
)

# If necessary it's possible to use VersionHelper from different location instead of downloaded from GitHub
# string(TOUPPER VersionHelper UP_VersionHelper)
# set(FETCHCONTENT_SOURCE_DIR_${UP_VersionHelper} ${CMAKE_SOURCE_DIR}/../VersionHelper)

FetchContent_MakeAvailable(VersionHelper strong_type user_literals endian utils buffer boost_leaf boost_pfr interval)
target_link_libraries(rabbit INTERFACE strong_type user_literals endian utils buffer boost_leaf boost_pfr interval)

# If use IDE add strong_type target into deps source group
set_target_properties(strong_type user_literals endian utils buffer boost_leaf boost_pfr interval PROPERTIES FOLDER deps)

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
