! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains some built-in checkers
module fortuno_checkers
  use fortuno_chartypes, only : char_rep_int, named_details, named_item
  use fortuno_testinfo, only : check_result
  implicit none

  private
  public :: is_equal


  !> Checks whether two entities are equal
  interface is_equal
    module procedure is_equal_i0_i0
  end interface is_equal

contains


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
      ! Workaround:gfortran:14.1 (bug 116679)
      ! Omit array expression to avoid memory leak
      ! {-
      ! checkresult%details = named_details([&
      !     & named_item("failure", "mismatching integer values"),&
      !     & named_item("expected", char_rep_int(expected)),&
      !     & named_item("obtained", char_rep_int(obtained))&
      !     & ])
      ! -}{+
      block
        type(named_details), allocatable :: nameddetails
        allocate(nameddetails)
        allocate(nameddetails%items(3))
        associate (items => nameddetails%items)
          items(1) = named_item("failure", "mismatching integer values")
          items(2) = named_item("expected", char_rep_int(expected))
          items(3) = named_item("obtained", char_rep_int(obtained))
        end associate
        call move_alloc(nameddetails, checkresult%details)
      end block
      ! +}
    end if

  end function is_equal_i0_i0

end module fortuno_checkers