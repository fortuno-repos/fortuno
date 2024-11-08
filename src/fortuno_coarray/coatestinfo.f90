! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains the coarray-extensions of test info structures
module fortuno_coarray_coatestinfo
  use fortuno, only : str, init_failure_location, failure_location, nl
  implicit none

  private
  public :: init_coa_failure_location, coa_failure_location


  !> Adds info about failed nodes to failure location
  type, extends(failure_location) :: coa_failure_location

    !> Image index of failed images
    integer, allocatable :: failedimages(:)

  contains
    procedure :: str => coa_failure_location_str
  end type coa_failure_location

contains


  !> Initializes an coa_failure_location instance
  subroutine init_coa_failure_location(this, failedimages, checknr, file, line)

    !> Instance
    type(coa_failure_location), intent(out) :: this

    !> List of failed ranks
    integer, intent(in) :: failedimages(:)

    !> Nr. of checks made so far
    integer, intent(in) :: checknr

    !> File where failure occured (if available)
    character(*), optional, intent(in) :: file

    !> Line where failure occured (if available)
    integer, optional, intent(in) :: line

    call init_failure_location(this%failure_location, checknr, file, line)
    this%failedimages = failedimages

  end subroutine init_coa_failure_location


  !> Character representation of the failure location
  function coa_failure_location_str(this) result(repr)

    !> Instance
    class(coa_failure_location), intent(in) :: this

    !> Character representation
    character(:), allocatable :: repr

    integer :: firstfailed, totalfailed

    repr = this%failure_location%as_string()
    if (.not. allocated(this%failedimages)) return
    firstfailed = this%failedimages(1)
    totalfailed = size(this%failedimages)
    if (totalfailed > 1) then
      repr = "Image: " // str(firstfailed) // " (+ " // str(totalfailed - 1) // " more)"&
          & // nl // repr
    else
      repr = "Image: " // str(firstfailed) // nl // repr
    end if

  end function coa_failure_location_str

end module fortuno_coarray_coatestinfo
