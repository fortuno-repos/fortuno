! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains some built-in checkers
module fortuno_checkers
  use fortuno_namedtypes, only : named_details, named_item, char_repr_int
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
      checkresult%details = named_details([&
          & named_item("failure", "mismatching integer values"),&
          & named_item("expected", char_repr_int(expected)),&
          & named_item("obtained", char_repr_int(obtained))&
          & ])
    end if

  end function is_equal_i0_i0

end module fortuno_checkers