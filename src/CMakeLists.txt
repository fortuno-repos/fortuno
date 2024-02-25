# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

target_sources(
  Fortuno PRIVATE
  fortuno.f90
)
add_subdirectory(fortuno)

# Store generated mod-files in a separate folder
set(moduledir "${CMAKE_CURRENT_BINARY_DIR}/mods")
set_target_properties(
  Fortuno PROPERTIES
  Fortran_MODULE_DIRECTORY "${moduledir}"
)

# Add mod-file directory to include search path for targets depending on the library
target_include_directories(
  Fortuno PUBLIC
  $<BUILD_INTERFACE:${moduledir}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_LIBDIR}/${FORTUNO_INSTALL_MODULEDIR}>
)

if (FORTUNO_INSTALL)
  install(
    TARGETS Fortuno
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
