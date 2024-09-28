! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains the MPI-extensions of test info structures
module fortuno_mpi_mpitestinfo
  use fortuno, only : as_char, init_failure_location, failure_location, nl
  implicit none

  private
  public :: init_mpi_failure_location, mpi_failure_location


  !> Adds info about failed nodes to failure location
  type, extends(failure_location) :: mpi_failure_location

    !> Mask for failed nodes
    integer, allocatable :: failedranks(:)

  contains
    procedure :: as_char => mpi_failure_location_as_char
  end type mpi_failure_location

contains


  !> Initializes an mpi_failure_location instance
  subroutine init_mpi_failure_location(this, failedranks, checknr, file, line)

    !> Instance
    type(mpi_failure_location), intent(out) :: this

    !> List of failed ranks
    integer, intent(in) :: failedranks(:)

    !> Nr. of checks made so far
    integer, intent(in) :: checknr

    !> File where failure occured (if available)
    character(*), optional, intent(in) :: file

    !> Line where failure occured (if available)
    integer, optional, intent(in) :: line

    call init_failure_location(this%failure_location, checknr, file, line)
    this%failedranks = failedranks

  end subroutine init_mpi_failure_location


  !> Character representation of the failure location
  function mpi_failure_location_as_char(this) result(repr)

    !> Instance
    class(mpi_failure_location), intent(in) :: this

    !> Character representation
    character(:), allocatable :: repr

    integer :: firstfailed, totalfailed

    repr = this%failure_location%as_char()
    if (.not. allocated(this%failedranks)) return
    firstfailed = this%failedranks(1)
    totalfailed = size(this%failedranks)
    if (totalfailed > 1) then
      repr = "Rank: " // as_char(firstfailed) // " (+ " // as_char(totalfailed - 1) // " more)"&
          & // nl // repr
    else
      repr = "Rank: " // as_char(firstfailed) // nl // repr
    end if

  end function mpi_failure_location_as_char

end module fortuno_mpi_mpitestinfo
