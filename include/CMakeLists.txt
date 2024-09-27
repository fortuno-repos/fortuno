# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

# Introduce a fake target associated with the location of the folder containing the include file.
# Query the INTERFACE_INCLUDE_DIRECTORIES of the target to obtain the include folder location.
add_library(fortuno_include_dir INTERFACE)
target_include_directories(
  fortuno_include_dir INTERFACE
  $<BUILD_INTERFACE:${CMAKE_CURRENT_SOURCE_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)
add_library(Fortuno::fortuno_include_dir ALIAS fortuno_include_dir)

if (FORTUNO_INSTALL)
  install(
    TARGETS fortuno_include_dir
    EXPORT FortunoTargets
  )
  if (FORTUNO_WITH_SERIAL)
    install(
      FILES fortuno_serial.fypp
      DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
      COMPONENT Fortuno_development
    )
  endif ()
  if (FORTUNO_WITH_MPI)
    install(
      FILES fortuno_mpi.fypp
      DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
      COMPONENT Fortuno_development
    )
  endif ()
endif ()