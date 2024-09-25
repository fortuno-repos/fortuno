# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

target_sources(
  fortuno_mpi
  PRIVATE
  mpibasetypes.f90
  mpicmdapp.f90
  mpiconlogger.f90
  mpicontext.f90
  mpidriver.f90
  mpienv.f90
  mpiglobalctx.f90
  mpicase.f90
  mpitestinfo.f90
  mpisuite.f90
)
