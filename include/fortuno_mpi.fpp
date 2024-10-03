! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

#ifndef __FORTUNO_MPI_FPP__
#define __FORTUNO_MPI_FPP__

#define CHECK(cond)\
  block;\
    use fortuno_mpi, only : mpi_check;\
    call mpi_check(cond, file=__FILE__, line=__LINE__);\
  end block;

#define CHECK_MSG(cond, msg)\
  block;\
    use fortuno_mpi, only : mpi_check;\
    call mpi_check(cond, msg, file=__FILE__, line=__LINE__);\
  end block;

#define ASSERT(cond)\
  block;\
    use fortuno_mpi, only : mpi_check, mpi_check_failed;\
    call mpi_check(cond, file=__FILE__, line=__LINE__);\
    if (mpi_check_failed()) return;\
  end block;

#define ASSERT_MSG(cond, msg)\
  block;\
    use fortuno_mpi, only : mpi_check, mpi_check_failed;\
    call mpi_check(cond, msg, file=__FILE__, line=__LINE__);\
    if (mpi_check_failed()) return;\
  end block;

#endif
