! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Demo module/library to be tested
module mylib
  use mpi_f08, only : MPI_Allreduce, MPI_Bcast, MPI_Comm, MPI_COMM_WORLD, MPI_IN_PLACE,&
      & MPI_INTEGER, MPI_SUM
  implicit none

  private
  public :: allreduce_sum, broadcast

contains


  !> Broadcasts a scalar integer
  subroutine broadcast(comm, buffer, source)

    !> MPI communicator to use
    type(MPI_Comm), intent(in) :: comm

    !> Item to broadcast
    integer, intent(inout) :: buffer

    !> Source rank (default = 0)
    integer, optional, intent(in) :: source

    integer :: source_
    integer :: ierror

    if (present(source)) then
      source_ = source
    else
      source_ = 0
    end if
    call MPI_Bcast(buffer, 1, MPI_INTEGER, source_, comm, ierror)
    if (ierror /= 0) error stop "MPI_Bcast failed in mylib/broadcast"

  end subroutine broadcast


  !> Reduces a scalar integer by summation on all ranks
  subroutine allreduce_sum(comm, val)

    !> MPI communicator
    type(MPI_Comm), intent(in) :: comm

    !> Value to reduce by summation
    integer, intent(inout) :: val

    integer :: ierror

    call MPI_Allreduce(MPI_IN_PLACE, val, 1, MPI_INTEGER, MPI_SUM, comm, ierror)
    if (ierror /= 0) error stop "MPI_Allreduce failed in mylib/allreduce_sum"

  end subroutine allreduce_sum

end module mylib
