! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

module test_checkers
  use fortuno_chartypes, only : char_rep, char_rep_int, named_details
  use fortuno_checkers, only : is_equal
  use fortuno_testinfo, only : check_result
  use fortuno_serial, only : test => serial_case_item, check => serial_check,&
      & failed => serial_failed, suite => serial_suite_item, test_item
  implicit none

  private
  public :: checkers_test_items

contains


  subroutine test_i0_i0_success()
    integer, parameter :: value1 = 9, value2 = 9
    type(check_result) :: res

    res = is_equal(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_i0_i0_success


  subroutine test_i0_i0_failure()
    integer, parameter :: value1 = 7, value2 = 9
    type(check_result), target :: res
    type(named_details), pointer :: pnamed

    res = is_equal(value1, value2)
    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    pnamed => named_details_ptr_(res%details)
    call check(associated(pnamed), msg="Expected named_details type, obtained something else")
    if (failed()) return

    call check(size(pnamed%items) == 3)
    if (failed()) return

    call check(pnamed%items(1)%name == "failure")
    select type (val => pnamed%items(1)%value)
    type is (character(*))
      call check(val == "mismatching integer values")
    end select

    call check(pnamed%items(2)%name == "value1")
    select type (val => pnamed%items(2)%value)
    type is (char_rep_int)
      call check(val%value == value1)
    end select

    call check(pnamed%items(3)%name == "value2")
    select type (val => pnamed%items(3)%value)
    type is (char_rep_int)
      call check(val%value == value2)
    end select

  end subroutine test_i0_i0_failure


  function checkers_test_items() result(testitems)
    type(test_item), allocatable :: testitems(:)

    testitems = [&
        suite("isequal", [&
            test("i0_i0_success", test_i0_i0_success),&
            test("i0_i0_failure", test_i0_i0_failure)&
        ])&
    ]

  end function checkers_test_items


  function named_details_ptr_(obj) result(pnamed)
    class(char_rep), pointer, intent(in) :: obj
    type(named_details), pointer :: pnamed

    select type (obj)
    type is (named_details)
      pnamed => obj
    class default
      pnamed => null()
    end select

  end function named_details_ptr_

end module test_checkers
