! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Global serial context to avoid explicit passing of context when using non-threaded serial driver
module fortuno_mpi_mpiglobalctx
  use mpi_f08, only : mpi_comm
  use fortuno, only : check_result, test_ptr_item
  use fortuno_mpi_mpicontext, only : mpi_context
  implicit none

  private
  public :: mpiglobalctx
  public :: set_mpi_global_context
  public :: mpi_check, mpi_check_failed, mpi_failed, mpi_skip
  public :: mpi_scope_pointers
  public :: global_comm, global_comm_id
  public :: num_ranks, this_rank


  !> Generic check interface accessing the global context
  interface mpi_check
    module procedure mpi_check_logical, mpi_check_check_result
  end interface mpi_check


  !> The global context
  type(mpi_context), pointer, protected :: mpiglobalctx => null()

contains


  !> Sets the global context
  subroutine set_mpi_global_context(newctx, oldctx)

    !> New context to use as global context
    type(mpi_context), pointer, intent(in) :: newctx

    !> Old context used so far as global context
    type(mpi_context), pointer, optional, intent(out) :: oldctx

    if (present(oldctx)) oldctx => mpiglobalctx
    mpiglobalctx => newctx

  end subroutine set_mpi_global_context


  !> Check using global context logical input
  subroutine mpi_check_logical(cond, msg, file, line)

    !> Whether check condition is fulfilled (check is successful)
    logical, intent(in) :: cond

    !> Check message
    character(*), optional, intent(in) :: msg

    !> Source file name
    character(*), optional, intent(in) :: file

    !> Line information
    integer, optional, intent(in) :: line

    call mpiglobalctx%check(cond, msg=msg, file=file, line=line)

  end subroutine mpi_check_logical


  !> Check using global context check_result input
  subroutine mpi_check_check_result(checkresult, msg, file, line)

    !> Whether check condition is fulfilled (check is successful)
    type(check_result), intent(in) :: checkresult

    !> Check message
    character(*), optional, intent(in) :: msg

    !> Source file name
    character(*), optional, intent(in) :: file

    !> Line information
    integer, optional, intent(in) :: line

    call mpiglobalctx%check(checkresult, msg=msg, file=file, line=line)

  end subroutine mpi_check_check_result


  !> Whether the last check has failed
  function mpi_check_failed() result(check_failed)

    !> True, if last check has failed
    logical :: check_failed

    check_failed = mpiglobalctx%check_failed()

  end function mpi_check_failed


  !> Whether the test/context has failed
  function mpi_failed() result(failed)

    !> True, if test has failed already (e.g. due to previous failing checks)
    logical :: failed

    failed = mpiglobalctx%failed()

  end function mpi_failed


  !> Mark current test/context as skipped
  subroutine mpi_skip()

    call mpiglobalctx%skip()

  end subroutine mpi_skip


  !> Returns the enclosing suite pointers
  function mpi_scope_pointers() result(scopeptrs)

    !> Pointers to enclosing suites
    type(test_ptr_item), allocatable :: scopeptrs(:)

    scopeptrs = mpiglobalctx%scope_pointers()

  end function mpi_scope_pointers


  !> Returns the global communicator
  function global_comm() result(comm)

    !> Global communicator
    type(mpi_comm) :: comm

    comm = mpiglobalctx%mpienv%comm

  end function global_comm


  !> Returns the global communicators integer id (to be used in F77/F90 MPI routines)
  function global_comm_id() result(id)

    integer :: id

    id = mpiglobalctx%mpienv%comm%mpi_val

  end function global_comm_id


  !> Returns the rank of the current process
  function this_rank() result(rank)

    !> Rank
    integer :: rank

    rank = mpiglobalctx%mpienv%rank

  end function this_rank


  !> Returns the number of all ranks in current global communicator
  function num_ranks() result(ranks)

    !> Nr. of ranks
    integer :: ranks

    ranks = mpiglobalctx%mpienv%nranks

  end function num_ranks

end module fortuno_mpi_mpiglobalctx
