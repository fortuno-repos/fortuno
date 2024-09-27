# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

list(APPEND CMAKE_MESSAGE_CONTEXT Example)

if (FORTUNO_BUILD_SERIAL_INTERFACE)
  add_subdirectory(serial)
  if (FYPP)
    add_subdirectory(serial-fypp)
  endif ()
endif ()

if (FORTUNO_BUILD_MPI_INTERFACE)
  add_subdirectory(mpi)
  if (FYPP)
    add_subdirectory(mpi-fypp)
  endif ()
endif ()

if (FORTUNO_BUILD_COARRAY_INTERFACE)
  add_subdirectory(coarray)
  # if (FYPP)
  #   add_subdirectory(mpi-fypp)
  # endif ()
endif ()
