include(FetchContent)

# If dependencies are not available then download it and make available
FetchContent_Declare(
	VersionHelper
    GIT_REPOSITORY https://github.com/rokoDev/VersionHelper.git
    GIT_TAG        eb8ee4d0ad5c8730adc39e73e04681453bcdc0c5
)

FetchContent_Declare(
	strong_type
	GIT_REPOSITORY https://github.com/rokoDev/strong_type.git
	GIT_TAG        512cdd6470a658f508a6fc9938de6dae28ff2b23
)

FetchContent_Declare(
	user_literals
	GIT_REPOSITORY https://github.com/rokoDev/user_literals.git
	GIT_TAG        e750e9879bfe717a8b64601a2a1d0a76e47916cd
)

FetchContent_Declare(
	endian
	GIT_REPOSITORY https://github.com/rokoDev/endian.git
	GIT_TAG        d1a314b7837cc661bc319f6e417d8e27ed5835c6
)

FetchContent_Declare(
	utils
	GIT_REPOSITORY https://github.com/rokoDev/utils.git
	GIT_TAG        4fdd621eebcd9e0109a715bcce7c9b9c6fb8b347
)

FetchContent_Declare(
	buffer
	GIT_REPOSITORY https://github.com/rokoDev/buffer.git
	GIT_TAG        f228c39f8c456e0700cd629360a35e81c300c6a5
)

FetchContent_Declare(
	boost_leaf
	GIT_REPOSITORY https://github.com/boostorg/leaf.git
	GIT_TAG        d8aae791332a1307ee5a14561a9f7636649e62b9
)

FetchContent_Declare(
	boost_pfr
	GIT_REPOSITORY https://github.com/boostorg/pfr.git
	GIT_TAG        98789f610a8bab53d5523157ff6a3fac7606764a
)

FetchContent_Declare(
	interval
	GIT_REPOSITORY https://github.com/rokoDev/interval.git
	GIT_TAG        ab70895cf999a0584024a718d7ea0a5d52bd4c52
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
