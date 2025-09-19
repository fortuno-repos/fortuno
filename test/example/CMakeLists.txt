# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

if (FORTUNO_WITH_SERIAL)
  Fortuno_add_test(serial
    TEST_NAME example-serial
  )
  if (FORTUNO_WITH_FPP)
    Fortuno_add_test(serial-fpp
      TEST_NAME example-serial-fpp
    )
  endif ()
  if (FYPP)
    Fortuno_add_test(serial-fypp
      TEST_NAME example-serial-fypp
    )
  endif ()
endif ()

if (FORTUNO_WITH_MPI)
  Fortuno_add_test(mpi
    TEST_NAME example-mpi
  )
  if (FORTUNO_WITH_FPP)
    Fortuno_add_test(mpi-fpp
      TEST_NAME example-mpi-fpp
    )
  endif ()
  if (FYPP)
    Fortuno_add_test(mpi-fypp
      TEST_NAME example-mpi-fypp
    )
  endif ()
endif ()

if (FORTUNO_WITH_COARRAY)
  Fortuno_add_test(coarray
    TEST_NAME example-coarray
  )
  if (FORTUNO_WITH_FPP)
    Fortuno_add_test(coarray-fpp
      TEST_NAME example-coarray-fpp
    )
  endif ()
  if (FYPP)
    Fortuno_add_test(coarray-fypp
      TEST_NAME example-coarray-fypp
    )
  endif ()
endif ()
