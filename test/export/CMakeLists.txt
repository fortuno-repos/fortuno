# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

if (FORTUNO_WITH_SERIAL)
    Fortuno_add_test(serial
        TEST_NAME export-serial
    )
endif ()

if (FORTUNO_WITH_MPI)
    Fortuno_add_test(mpi
        TEST_NAME export-mpi
    )
endif ()

if (FORTUNO_WITH_COARRAY)
    Fortuno_add_test(coarray
        TEST_NAME export-coarray
    )
endif ()