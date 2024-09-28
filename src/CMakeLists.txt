# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

set(_installed_targets)

# Folder for generated mod-files
set(moduledir "${CMAKE_CURRENT_BINARY_DIR}/modules")

add_library(fortuno)
set_target_properties(
  fortuno
  PROPERTIES
    VERSION ${PROJECT_VERSION}
    # TODO: change to ${PROJECT_VERSION_MAJOR} once project version reaches 1.0
    SOVERSION ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
    EXPORT_NAME fortuno
    OUTPUT_NAME fortuno
    Fortran_MODULE_DIRECTORY "${moduledir}"
)
fortuno_add_thread_safe_build_flags(fortuno)
target_include_directories(
  fortuno PUBLIC
  $<BUILD_INTERFACE:${moduledir}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_LIBDIR}/${FORTUNO_INSTALL_MODULEDIR}>
)
target_sources(fortuno PRIVATE fortuno.f90)
add_subdirectory(fortuno)

add_library(Fortuno::fortuno ALIAS fortuno)
list(APPEND _installed_targets "fortuno")

if (FORTUNO_WITH_SERIAL)

  add_library(fortuno_serial)
  set_target_properties(
    fortuno_serial
    PROPERTIES
      VERSION ${PROJECT_VERSION}
      # TODO: change to ${PROJECT_VERSION_MAJOR} once project version reaches 1.0
      SOVERSION ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
      EXPORT_NAME fortuno_serial
      OUTPUT_NAME fortuno-serial
      Fortran_MODULE_DIRECTORY "${moduledir}"
  )
  target_link_libraries(fortuno_serial PUBLIC Fortuno::fortuno)
  target_include_directories(
    fortuno_serial PUBLIC
    $<BUILD_INTERFACE:${moduledir}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_LIBDIR}/${FORTUNO_INSTALL_MODULEDIR}>
  )
  target_sources(fortuno_serial PRIVATE fortuno_serial.f90)
  add_subdirectory(fortuno_serial)

  add_library(Fortuno::fortuno_serial ALIAS fortuno_serial)
  list(APPEND _installed_targets "fortuno_serial")

endif ()


if (FORTUNO_WITH_MPI)

  add_library(fortuno_mpi)
  set_target_properties(
    fortuno_mpi PROPERTIES
    VERSION ${PROJECT_VERSION}
    # TODO: change to ${PROJECT_VERSION_MAJOR} once project version reaches 1.0
    SOVERSION ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
    EXPORT_NAME fortuno_mpi
    OUTPUT_NAME fortuno-mpi
    Fortran_MODULE_DIRECTORY "${moduledir}"
  )
  target_link_libraries(fortuno_mpi PRIVATE MPI::MPI_Fortran)
  target_link_libraries(fortuno_mpi PUBLIC Fortuno::fortuno)
  target_include_directories(
    fortuno_mpi PUBLIC
    $<BUILD_INTERFACE:${moduledir}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_LIBDIR}/${FORTUNO_INSTALL_MODULEDIR}>
  )
  target_sources(fortuno_mpi PRIVATE fortuno_mpi.f90)
  add_subdirectory(fortuno_mpi)

  add_library(Fortuno::fortuno_mpi ALIAS fortuno_mpi)
  list(APPEND _installed_targets "fortuno_mpi")

endif ()


if (FORTUNO_WITH_COARRAY)

  add_library(fortuno_coarray)
  set_target_properties(
    fortuno_coarray PROPERTIES
    VERSION ${PROJECT_VERSION}
    # TODO: change to ${PROJECT_VERSION_MAJOR} once project version reaches 1.0
    SOVERSION ${PROJECT_VERSION_MAJOR}.${PROJECT_VERSION_MINOR}
    EXPORT_NAME fortuno_coarray
    OUTPUT_NAME fortuno-coarray
    Fortran_MODULE_DIRECTORY "${moduledir}"
  )
  fortuno_add_coarray_build_flags(fortuno_coarray)
  target_link_libraries(fortuno_coarray PUBLIC Fortuno::fortuno)
  target_include_directories(
    fortuno_coarray PUBLIC
    $<BUILD_INTERFACE:${moduledir}>
    $<INSTALL_INTERFACE:${CMAKE_INSTALL_LIBDIR}/${FORTUNO_INSTALL_MODULEDIR}>
  )
  target_sources(fortuno_coarray PRIVATE fortuno_coarray.f90)
  add_subdirectory(fortuno_coarray)

  add_library(Fortuno::fortuno_coarray ALIAS fortuno_coarray)
  list(APPEND _installed_targets "fortuno_coarray")

endif ()


if (FORTUNO_INSTALL)
  install(
    TARGETS ${_installed_targets}
    EXPORT FortunoTargets
    LIBRARY
      DESTINATION "${CMAKE_INSTALL_LIBDIR}"
      COMPONENT Fortuno_runtime
      NAMELINK_COMPONENT Fortuno_development
    PUBLIC_HEADER
      DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
      COMPONENT Fortuno_development
  )
  install(
    DIRECTORY "${moduledir}/"
    DESTINATION "${CMAKE_INSTALL_LIBDIR}/${FORTUNO_INSTALL_MODULEDIR}"
    COMPONENT Fortuno_development
  )
endif ()
