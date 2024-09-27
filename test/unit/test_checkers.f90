! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

module test_checkers
  use fortuno_chartypes, only : char_rep, char_rep_int, char_rep_int_r1, named_details
  use fortuno_checkers, only : is_equal, int_r1_item
  use fortuno_testinfo, only : check_result
  use fortuno_serial, only : test => serial_case_item, check => serial_check,&
      & failed => serial_failed, suite => serial_suite_item, test_item
  implicit none

  private
  public :: checkers_test_items

contains


  subroutine test_isequal_i0_i0_success()
    integer, parameter :: value1 = 9, value2 = 9
    type(check_result) :: res

    res = is_equal(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_isequal_i0_i0_success


  subroutine test_isequal_i0_i0_mismatch()
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
    select type (itemval => pnamed%items(1)%value)
    type is (character(*))
      call check(itemval == "mismatching values")
    end select

    call check(pnamed%items(2)%name == "value1")
    select type (itemval => pnamed%items(2)%value)
    type is (char_rep_int)
      call check(itemval%value == value1)
    end select

    call check(pnamed%items(3)%name == "value2")
    select type (itemval => pnamed%items(3)%value)
    type is (char_rep_int)
      call check(itemval%value == value2)
    end select

  end subroutine test_isequal_i0_i0_mismatch


  subroutine test_isequal_i1_i1_success()
    integer, parameter :: value1(2) = [1, 2], value2(2) = [1, 2]
    type(check_result) :: res

    res = is_equal(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_isequal_i1_i1_success


  subroutine test_isequal_i1_i1_mismatch_shape()
    integer, parameter :: value1(2) = [3, 4], value2(3) = [3, 4, -1]
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
    select type (itemval => pnamed%items(1)%value)
    type is (character(*))
      call check(itemval == "mismatching array shapes")
    end select

    call check(pnamed%items(2)%name == "shape1")
    select type (itemval => pnamed%items(2)%value)
    type is (char_rep_int_r1)
      call check(all(itemval%value == shape(value1)))
    end select

    call check(pnamed%items(3)%name == "shape2")
    select type (itemval => pnamed%items(3)%value)
    type is (char_rep_int_r1)
      call check(all(itemval%value == shape(value2)))
    end select

  end subroutine test_isequal_i1_i1_mismatch_shape


  subroutine test_isequal_i1_i1_mismatch_value()
    integer, parameter :: value1(2) = [3, 4], value2(2) = [3, -4], mismatchloc(1) = [2]
    type(check_result), target :: res
    type(named_details), pointer :: pnamed

    res = is_equal(value1, value2)
    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    pnamed => named_details_ptr_(res%details)
    call check(associated(pnamed), msg="Expected named_details type, obtained something else")
    if (failed()) return

    call check(size(pnamed%items) == 4)
    if (failed()) return

    call check(pnamed%items(1)%name == "failure")
    select type (itemval => pnamed%items(1)%value)
    type is (character(*))
      call check(itemval == "mismatching array elements")
    end select

    call check(pnamed%items(2)%name == "location")
    select type (itemval => pnamed%items(2)%value)
    type is (int_r1_item)
      call check(size(itemval%value) == size(mismatchloc))
      if (failed()) return
      call check(all(itemval%value == mismatchloc))
    end select

    call check(pnamed%items(3)%name == "value1")
    select type (itemval => pnamed%items(3)%value)
    type is (char_rep_int)
      call check(itemval%value == value1(mismatchloc(1)))
    end select

    call check(pnamed%items(4)%name == "value2")
    select type (itemval => pnamed%items(4)%value)
    type is (char_rep_int)
      call check(itemval%value == value2(mismatchloc(1)))
    end select

  end subroutine test_isequal_i1_i1_mismatch_value


  subroutine test_isequal_i2_i2_success()
    integer, parameter :: value1(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    integer, parameter :: value2(2, 2) = value1
    type(check_result) :: res

    res = is_equal(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_isequal_i2_i2_success


  subroutine test_isequal_i2_i2_mismatch_shape()
    integer, parameter :: value1(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    integer, parameter :: value2(2, 3) = reshape([1, 2, 3, 4, 5, 6], [2, 3])
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
    select type (itemval => pnamed%items(1)%value)
    type is (character(*))
      call check(itemval == "mismatching array shapes")
    end select

    call check(pnamed%items(2)%name == "shape1")
    select type (itemval => pnamed%items(2)%value)
    type is (char_rep_int_r1)
      call check(all(itemval%value == shape(value1)))
    end select

    call check(pnamed%items(3)%name == "shape2")
    select type (itemval => pnamed%items(3)%value)
    type is (char_rep_int_r1)
      call check(all(itemval%value == shape(value2)))
    end select

  end subroutine test_isequal_i2_i2_mismatch_shape


  subroutine test_isequal_i2_i2_mismatch_value()
    integer, parameter :: value1(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    integer, parameter :: value2(2, 2) = reshape([1, 2, -3, 4], [2, 2])
    integer, parameter :: mismatchloc(2) = [1, 2]
    type(check_result), target :: res
    type(named_details), pointer :: pnamed

    res = is_equal(value1, value2)
    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    pnamed => named_details_ptr_(res%details)
    call check(associated(pnamed), msg="Expected named_details type, obtained something else")
    if (failed()) return

    call check(size(pnamed%items) == 4)
    if (failed()) return

    call check(pnamed%items(1)%name == "failure")
    select type (itemval => pnamed%items(1)%value)
    type is (character(*))
      call check(itemval == "mismatching array elements")
    end select

    call check(pnamed%items(2)%name == "location")
    select type (itemval => pnamed%items(2)%value)
    type is (int_r1_item)
      call check(size(itemval%value) == size(mismatchloc))
      if (failed()) return
      call check(all(itemval%value == mismatchloc))
    end select

    call check(pnamed%items(3)%name == "value1")
    select type (itemval => pnamed%items(3)%value)
    type is (char_rep_int)
      call check(itemval%value == value1(mismatchloc(1), mismatchloc(2)))
    end select

    call check(pnamed%items(4)%name == "value2")
    select type (itemval => pnamed%items(4)%value)
    type is (char_rep_int)
      call check(itemval%value == value2(mismatchloc(1), mismatchloc(2)))
    end select

  end subroutine test_isequal_i2_i2_mismatch_value


  function checkers_test_items() result(testitems)
    type(test_item), allocatable :: testitems(:)

    testitems = [&
        suite("checkers", [&
            suite("isequal", [&
                test("i0_i0_success", test_isequal_i0_i0_success),&
                test("i0_i0_mismatch", test_isequal_i0_i0_mismatch),&
                test("i1_i1_success", test_isequal_i1_i1_success),&
                test("i1_i1_mismatch_shape", test_isequal_i1_i1_mismatch_shape),&
                test("i1_i1_mismatch_value", test_isequal_i1_i1_mismatch_value),&
                test("i2_i2_success", test_isequal_i2_i2_success),&
                test("i2_i2_mismatch_shape", test_isequal_i2_i2_mismatch_shape),&
                test("i2_i2_mismatch_value", test_isequal_i2_i2_mismatch_value)&
            ])&
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
