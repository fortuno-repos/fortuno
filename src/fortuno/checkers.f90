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
  function is_equal_i0_i0(value1, value2) result(checkresult)

    !> First value to check
    integer, intent(in) :: value1

    !> Second value to check
    integer, intent(in) :: value2

    !> Result of the check
    type(check_result) :: checkresult

    checkresult%success = (value1 == value2)
    if (.not. checkresult%success) then
      checkresult%details = named_details([&
          & named_item("failure", "mismatching integer values"),&
          & named_item("value1", char_rep_int(value1)),&
          & named_item("value2", char_rep_int(value2))&
          & ])
    end if

  end function is_equal_i0_i0


end module fortuno_checkers