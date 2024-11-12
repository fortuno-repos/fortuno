# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

list(APPEND CMAKE_MESSAGE_CONTEXT Example)

if (FORTUNO_WITH_SERIAL)
  add_subdirectory(serial)
  if (FORTUNO_WITH_FPP)
    add_subdirectory(serial-fpp)
  endif ()
  if (FYPP)
    add_subdirectory(serial-fypp)
  endif ()
endif ()

if (FORTUNO_WITH_MPI)
  add_subdirectory(mpi)
  if (FORTUNO_WITH_FPP)
    add_subdirectory(mpi-fpp)
  endif ()
  if (FYPP)
    add_subdirectory(mpi-fypp)
  endif ()
endif ()

if (FORTUNO_WITH_COARRAY)
  add_subdirectory(coarray)
  if (FORTUNO_WITH_FPP)
    add_subdirectory(coarray-fpp)
  endif ()
  if (FYPP)
    add_subdirectory(coarray-fypp)
  endif ()
endif ()
