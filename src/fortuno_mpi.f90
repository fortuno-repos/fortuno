! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Exports the MPI-dependent part of Fortuno
module fortuno_mpi
  use fortuno
  use fortuno_mpi_mpicmdapp, only : mpi_cmd_app, execute_mpi_cmd_app
  use fortuno_mpi_mpicontext, only : mpi_context
  use fortuno_mpi_mpiglobalctx, only : global_comm, global_comm_id, mpi_check, mpi_check_failed,&
      & mpi_failed, mpi_skip, mpi_scope_pointers, num_ranks, this_rank
  use fortuno_mpi_mpicase, only : mpi_case, mpi_case_item
  use fortuno_mpi_mpisuite, only : mpi_suite, mpi_suite_item
  implicit none
end module
