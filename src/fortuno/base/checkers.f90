! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains some built-in checkers
module fortuno_base_checkers
  use fortuno_base_testinfo, only : check_result, failure_details
  use fortuno_base_utils, only : as_char, nl
  implicit none

  private
  public :: is_equal


  !> Checks whether two entities are equal
  interface is_equal
    module procedure is_equal_i0_i0
  end interface is_equal


  !> Details of an integer-integer check
  type, extends(failure_details) :: failure_details_i0_i0

    !> Value obtained
    integer :: obtained

    !> Value expected
    integer :: expected

  contains
    procedure :: as_char => failure_details_i0_i0_as_char
  end type failure_details_i0_i0

contains

  !> Character representation of an integer-integer check
  function failure_details_i0_i0_as_char(this) result(repr)

    !> Instance
    class(failure_details_i0_i0), intent(in) :: this

    !> Character representation of the instance
    character(:), allocatable :: repr

    repr = "Mismatching integer values" // nl&
        & // "Obtained: " // as_char(this%obtained) // nl&
        & // "Expected: " // as_char(this%expected)

  end function failure_details_i0_i0_as_char


  !> Checks whether two integer values are equal
  function is_equal_i0_i0(obtained, expected) result(checkresult)

    !> Obtained value
    integer, intent(in) :: obtained

    !> Expected value
    integer, intent(in) :: expected

    !> Result of the check
    type(check_result) :: checkresult

    checkresult%success = (obtained == expected)
    if (.not. checkresult%success) then
      checkresult%details = failure_details_i0_i0(obtained, expected)
    end if

  end function is_equal_i0_i0

end module fortuno_base_checkers