#[=================================================================================================[
#                                 Basic project definition                                        #
]=================================================================================================]

cmake_minimum_required(VERSION 3.22...3.28)

list(APPEND CMAKE_MESSAGE_CONTEXT Fortuno)
project(
  Fortuno
  VERSION 0.1.0
  DESCRIPTION "Extensible unit testing framework for Fortran"
  HOMEPAGE_URL "https://github.com/fortuno-repos/fortuno"
  LANGUAGES Fortran
)


#[=================================================================================================[
#                                 Options                                                         #
]=================================================================================================]

include(FeatureSummary)

option(FORTUNO_BUILD_SHARED_LIBS "Fortuno: Build as shared library" ${PROJECT_IS_TOP_LEVEL})

option(FORTUNO_BUILD_TESTS "Fortuno: Build test suite" ${PROJECT_IS_TOP_LEVEL})

option(FORTUNO_BUILD_EXAMPLES "Fortuno: Build example apps" ${PROJECT_IS_TOP_LEVEL})

option(FORTUNO_INSTALL "Fortuno: Install project" ${PROJECT_IS_TOP_LEVEL})

set(
  FORTUNO_INSTALL_MODULEDIR "modules" CACHE STRING
  "Sub-directory to install the Fortran module files into (relative to CMAKE_INSTALL_LIBDIR)"
)

#[=================================================================================================[
#                            Project configuration                                                #
]=================================================================================================]

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)
include(FortunoHelpers)

if (FORTUNO_INSTALL)
	include(CMakePackageConfigHelpers)
  include(GNUInstallDirs)
endif ()

fortuno_setup_build_type("RelWithDebInfo")
set(BUILD_SHARED_LIBS ${FORTUNO_BUILD_SHARED_LIBS})

# Report configuration
feature_summary(
  FILENAME ${CMAKE_CURRENT_BINARY_DIR}/Fortuno.info
	VAR Fortuno_Info
	DESCRIPTION "Fortuno features and external libraries"
	FATAL_ON_MISSING_REQUIRED_PACKAGES
	WHAT ALL
)
message(STATUS ${Fortuno_Info})


#[=================================================================================================[
#                                 Main definition                                                 #
]=================================================================================================]

add_library(Fortuno)
set_target_properties(
  Fortuno PROPERTIES
  VERSION ${PROJECT_VERSION}
  # SOVERSION ${PROJECT_VERSION_MAJOR}
  SOVERSION ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
  EXPORT_NAME Fortuno
  OUTPUT_NAME fortuno
)
add_library(Fortuno::Fortuno ALIAS Fortuno)

add_subdirectory(src)
if (FORTUNO_BUILD_EXAMPLES)
  add_subdirectory(example)
endif ()


#[=================================================================================================[
#                                 Install or Export                                               #
]=================================================================================================]

if (FORTUNO_INSTALL)

	# pkg-config files
	configure_file(cmake/fortuno.pc.in fortuno.pc @ONLY)
	install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/fortuno.pc
		DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
		COMPONENT Fortuno_development
  )

  # cmake export files
	write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/FortunoConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    # COMPATIBILITY SameMajorVersion
    COMPATIBILITY SameMinorVersion
  )
  configure_package_config_file(
      cmake/FortunoConfig.cmake.in
      ${CMAKE_CURRENT_BINARY_DIR}/FortunoConfig.cmake
      INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Fortuno
  )
  install(
    FILES ${CMAKE_CURRENT_BINARY_DIR}/FortunoConfigVersion.cmake
    ${CMAKE_CURRENT_BINARY_DIR}/FortunoConfig.cmake
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Fortuno
    COMPONENT Fortuno_development
  )

  export(
    EXPORT FortunoTargets
    FILE FortunoTargets.cmake
    NAMESPACE Fortuno::
  )
  install(
    EXPORT FortunoTargets
    FILE FortunoTargets.cmake
    NAMESPACE Fortuno::
    DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Fortuno
    COMPONENT Fortuno_development
  )
endif ()


# Make project available for FetchContent
if (NOT PROJECT_IS_TOP_LEVEL)
  # Propagate variables
	if (CMAKE_VERSION VERSION_LESS 3.25)
		# TODO: Remove when required minimum cmake version is >= 3.25
		set(Fortuno_VERSION ${Fortuno_VERSION} PARENT_SCOPE)
		set(Fortuno_VERSION_MAJOR ${Fortuno_VERSION_MAJOR} PARENT_SCOPE)
		set(Fortuno_VERSION_MINOR ${Fortuno_VERSION_MINOR} PARENT_SCOPE)
		set(Fortuno_VERSION_PATCH ${Fortuno_VERSION_PATCH} PARENT_SCOPE)
		set(Fortuno_VERSION_TWEAK ${Fortuno_VERSION_TWEAK} PARENT_SCOPE)
	else ()
		return(
      PROPAGATE
      Fortuno_VERSION
      Fortuno_VERSION_MAJOR
      Fortuno_VERSION_MINOR
			Fortuno_VERSION_PATCH
			Fortuno_VERSION_TWEAK
		)
	endif ()
endif ()
