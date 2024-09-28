# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

# The CMake config files were created based on the public template at
# https://github.com/LecrisUT/CMake-Template

#[=================================================================================================[
#                                 Basic project definition                                        #
]=================================================================================================]

cmake_minimum_required(VERSION 3.22...3.28)

list(APPEND CMAKE_MESSAGE_CONTEXT Fortuno)

project(
  Fortuno
  VERSION 0.1.0
  DESCRIPTION "Flextensible unit testing framework for Fortran"
  HOMEPAGE_URL "https://github.com/fortuno-repos/fortuno"
  LANGUAGES Fortran
)

#[=================================================================================================[
#                           Build configuration options                                           #
]=================================================================================================]

include(config.cmake)

#[=================================================================================================[
#                            Project configuration                                                #
]=================================================================================================]

list(APPEND CMAKE_MODULE_PATH ${PROJECT_SOURCE_DIR}/cmake)
include(FortunoHelpers)

if (FORTUNO_INSTALL)
  include(CMakePackageConfigHelpers)
  include(GNUInstallDirs)
endif ()

find_program(FYPP fypp)

set(BUILD_SHARED_LIBS ${FORTUNO_BUILD_SHARED_LIBS})
fortuno_setup_build_type("RelWithDebInfo")

if (FORTUNO_WITH_MPI)
  find_package(MPI REQUIRED)
  if(NOT MPI_FORTRAN_FOUND)
    message(FATAL_ERROR "Failed to detect MPI-framework for Fortran")
  endif()
endif ()

#[=================================================================================================[
#                                 Build definitions                                               #
]=================================================================================================]

add_subdirectory(include)
add_subdirectory(src)
if (FORTUNO_WITH_EXAMPLES)
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

  if (FORTUNO_WITH_SERIAL)
    configure_file(cmake/fortuno-serial.pc.in fortuno-serial.pc @ONLY)
    install(
      FILES ${CMAKE_CURRENT_BINARY_DIR}/fortuno-serial.pc
      DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
      COMPONENT Fortuno_development
    )
  endif ()

  if (FORTUNO_WITH_MPI)
    configure_file(cmake/fortuno-mpi.pc.in fortuno-mpi.pc @ONLY)
    install(
      FILES ${CMAKE_CURRENT_BINARY_DIR}/fortuno-mpi.pc
      DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
      COMPONENT Fortuno_development
    )
  endif ()

  if (FORTUNO_WITH_COARRAY)
    configure_file(cmake/fortuno-coarray.pc.in fortuno-coarray.pc @ONLY)
    install(
      FILES ${CMAKE_CURRENT_BINARY_DIR}/fortuno-coarray.pc
      DESTINATION ${CMAKE_INSTALL_LIBDIR}/pkgconfig
      COMPONENT Fortuno_development
    )
  endif ()

  # cmake export files
  write_basic_package_version_file(
    ${CMAKE_CURRENT_BINARY_DIR}/FortunoConfigVersion.cmake
    VERSION ${PROJECT_VERSION}
    # TODO: Switch to SameMajorVersion as soon as project version reaches 1.0.
    COMPATIBILITY SameMinorVersion
  )
  configure_package_config_file(
      cmake/FortunoConfig.cmake.in
      ${CMAKE_CURRENT_BINARY_DIR}/FortunoConfig.cmake
      INSTALL_DESTINATION ${CMAKE_INSTALL_LIBDIR}/cmake/Fortuno
  )
  install(
    FILES
      ${CMAKE_CURRENT_BINARY_DIR}/FortunoConfigVersion.cmake
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
