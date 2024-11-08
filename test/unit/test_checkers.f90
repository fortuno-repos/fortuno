! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

module test_checkers
  use iso_fortran_env, only : i32 => int32, i64 => int64, r32 => real32
  use fortuno_checkers, only : all_close, all_equal, is_close, is_equal
  use fortuno_serial, only : check_result, details_dict, get_ptr_to, matches_type_value,&
      & test => serial_case_item, check => serial_check, failed => serial_failed,&
      & suite => serial_suite_item, test_list
  implicit none

  private
  public :: tests

  character(*), parameter :: mismatch_shape_msg_ = "mismatching array shapes"
  character(*), parameter :: mismatch_value_msg_int_ = "mismatching integer values"
  character(*), parameter :: mismatch_value_msg_real_ = "real values differing beyond tolerance"

contains


  subroutine test_equal_i32_i32_success()
    integer(i32), parameter :: value1 = 9, value2 = 9
    type(check_result) :: res

    res = is_equal(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_equal_i32_i32_success


  subroutine test_equal_i32_i32_mismatch()
    integer(i32), parameter :: value1 = 7, value2 = 9
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = is_equal(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 3)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_value_msg_int_))

    call check(pdict%items(2)%name == "value1")
    call check(matches_type_value(pdict%items(2)%value, value1))

    call check(pdict%items(3)%name == "value2")
    call check(matches_type_value(pdict%items(3)%value, value2))

  end subroutine test_equal_i32_i32_mismatch


  subroutine test_equal_i32r1_i32r1_success()
    integer(i32), parameter :: value1(2) = [1, 2], value2(2) = [1, 2]
    type(check_result) :: res

    res = all_equal(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_equal_i32r1_i32r1_success


  subroutine test_equal_i32r1_i32r1_mismatch_shape()
    integer(i32), parameter :: value1(2) = [3, 4], value2(3) = [3, 4, -1]
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_equal(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 3)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_shape_msg_))

    call check(pdict%items(2)%name == "shape1")
    call check(matches_type_value(pdict%items(2)%value, shape(value1, kind=i64)))

    call check(pdict%items(3)%name == "shape2")
    call check(matches_type_value(pdict%items(3)%value, shape(value2, kind=i64)))

  end subroutine test_equal_i32r1_i32r1_mismatch_shape


  subroutine test_equal_i32r1_i32r1_mismatch_value()
    integer(i32), parameter :: value1(2) = [3, 4], value2(2) = [3, -4]
    integer(i64), parameter :: mismatchloc(1) = [2]
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_equal(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 4)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_value_msg_int_))

    call check(pdict%items(2)%name == "location")
    call check(matches_type_value(pdict%items(2)%value, mismatchloc))

    call check(pdict%items(3)%name == "value1")
    call check(matches_type_value(pdict%items(3)%value, value1(mismatchloc(1))))

    call check(pdict%items(4)%name == "value2")
    call check(matches_type_value(pdict%items(4)%value, value2(mismatchloc(1))))

  end subroutine test_equal_i32r1_i32r1_mismatch_value


  subroutine test_equal_i32r2_i32r2_success()
    integer(i32), parameter :: value1(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    integer(i32), parameter :: value2(2, 2) = value1
    type(check_result) :: res

    res = all_equal(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_equal_i32r2_i32r2_success


  subroutine test_equal_i32r2_i32r2_mismatch_shape()
    integer(i32), parameter :: value1(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    integer(i32), parameter :: value2(2, 3) = reshape([1, 2, 3, 4, 5, 6], [2, 3])
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_equal(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 3)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_shape_msg_))

    call check(pdict%items(2)%name == "shape1")
    call check(matches_type_value(pdict%items(2)%value, shape(value1, kind=i64)))

    call check(pdict%items(3)%name == "shape2")
    call check(matches_type_value(pdict%items(3)%value, shape(value2, kind=i64)))

  end subroutine test_equal_i32r2_i32r2_mismatch_shape


  subroutine test_equal_i32r2_i32r2_mismatch_value()
    integer(i32), parameter :: value1(2, 2) = reshape([1, 2, 3, 4], [2, 2])
    integer(i32), parameter :: value2(2, 2) = reshape([1, 2, -3, 4], [2, 2])
    integer(i64), parameter :: mismatchloc(2) = [1, 2]
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_equal(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 4)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_value_msg_int_))

    call check(pdict%items(2)%name == "location")
    call check(matches_type_value(pdict%items(2)%value, mismatchloc))

    call check(pdict%items(3)%name == "value1")
    call check(matches_type_value(pdict%items(3)%value, value1(mismatchloc(1), mismatchloc(2))))

    call check(pdict%items(4)%name == "value2")
    call check(matches_type_value(pdict%items(4)%value, value2(mismatchloc(1), mismatchloc(2))))

  end subroutine test_equal_i32r2_i32r2_mismatch_value


  subroutine test_equal_i32r1_i32r0_success()
    integer(i32), parameter :: value1(2) = [2, 2], value2 = 2
    type(check_result) :: res

    res = all_equal(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_equal_i32r1_i32r0_success


  subroutine test_equal_i32r0_i32r1_success()
    integer(i32), parameter :: value1 = 2, value2(2) = [2, 2]
    type(check_result) :: res

    res = all_equal(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_equal_i32r0_i32r1_success


  subroutine test_equal_i32r1_i32r0_mismatch()
    integer(i32), parameter :: value1(2) = [3, 4], value2 = 3
    integer(i64), parameter :: mismatchloc(1) = [2]
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_equal(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 4)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_value_msg_int_))

    call check(pdict%items(2)%name == "location")
    call check(matches_type_value(pdict%items(2)%value, mismatchloc))

    call check(pdict%items(3)%name == "value1")
    call check(matches_type_value(pdict%items(3)%value, value1(mismatchloc(1))))

    call check(pdict%items(4)%name == "value2")
    call check(matches_type_value(pdict%items(4)%value, value2))

  end subroutine test_equal_i32r1_i32r0_mismatch


  subroutine test_equal_i32r0_i32r1_mismatch
    integer(i32), parameter :: value1 = 3, value2(2) = [3, 4]
    integer(i64), parameter :: mismatchloc(1) = [2]
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_equal(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 4)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_value_msg_int_))

    call check(pdict%items(2)%name == "location")
    call check(matches_type_value(pdict%items(2)%value, mismatchloc))

    call check(pdict%items(3)%name == "value1")
    call check(matches_type_value(pdict%items(3)%value, value1))

    call check(pdict%items(4)%name == "value2")
    call check(matches_type_value(pdict%items(4)%value, value2(mismatchloc(1))))

  end subroutine test_equal_i32r0_i32r1_mismatch


  subroutine test_equal_i32r2_i32r0_success()
    integer(i32), parameter :: value1(2, 2) = reshape([9, 9, 9, 9], [2, 2]), value2 = 9
    type(check_result) :: res

    res = all_equal(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_equal_i32r2_i32r0_success


  subroutine test_equal_i32r0_i32r2_success()
    integer(i32), parameter :: value1 = 9, value2(2, 2) = reshape([9, 9, 9, 9], [2, 2])
    type(check_result) :: res

    res = all_equal(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_equal_i32r0_i32r2_success


  subroutine test_equal_i32r2_i32r0_mismatch()
    integer(i32), parameter :: value1(2, 2) = reshape([3, 3, 4, 3], [2, 2]), value2 = 3
    integer(i64), parameter :: mismatchloc(2) = [1, 2]
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_equal(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 4)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_value_msg_int_))

    call check(pdict%items(2)%name == "location")
    call check(matches_type_value(pdict%items(2)%value, mismatchloc))

    call check(pdict%items(3)%name == "value1")
    call check(matches_type_value(pdict%items(3)%value, value1(mismatchloc(1), mismatchloc(2))))

    call check(pdict%items(4)%name == "value2")
    call check(matches_type_value(pdict%items(4)%value, value2))

  end subroutine test_equal_i32r2_i32r0_mismatch


  subroutine test_equal_i32r0_i32r2_mismatch
    integer(i32), parameter :: value1 = 3, value2(2, 2) = reshape([3, 3, 4, 3], [2, 2])
    integer(i64), parameter :: mismatchloc(2) = [1, 2]
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_equal(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 4)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_value_msg_int_))

    call check(pdict%items(2)%name == "location")
    call check(matches_type_value(pdict%items(2)%value, mismatchloc))

    call check(pdict%items(3)%name == "value1")
    call check(matches_type_value(pdict%items(3)%value, value1))

    call check(pdict%items(4)%name == "value2")
    call check(matches_type_value(pdict%items(4)%value, value2(mismatchloc(1), mismatchloc(2))))

  end subroutine test_equal_i32r0_i32r2_mismatch


  subroutine test_close_r32_r32_success()
    real(r32), parameter :: value1 = 0.1_r32, value2 = value1 + 1.0e-5_r32
    type(check_result) :: res

    res = is_close(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_close_r32_r32_success


  subroutine test_close_r32_r32_mismatch()
    real(r32), parameter :: value1 = 0.1_r32, value2 = value1 + 1.0e-3_r32
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = is_close(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 3)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_value_msg_real_))

    call check(pdict%items(2)%name == "value1")
    call check(matches_type_value(pdict%items(2)%value, value1, rtol=1e-6_r32, atol=1e-6_r32))

    call check(pdict%items(3)%name == "value2")
    call check(matches_type_value(pdict%items(3)%value, value2, rtol=1e-6_r32, atol=1e-6_r32))

  end subroutine test_close_r32_r32_mismatch


  subroutine test_close_r32r1_r32r1_success()
    real(r32), parameter :: value1(2) = [0.1_r32, 2.2e-12_r32]
    real(r32), parameter :: value2(2) = value1 + [1.0e-5_r32, 5e-17_r32]
    type(check_result) :: res

    res = all_close(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_close_r32r1_r32r1_success


  subroutine test_close_r32r1_r32r1_mismatch_shape()
    real(r32), parameter :: value1(2) = [0.1_r32, 2.2e-12_r32]
    real(r32), parameter :: value2(1) = [0.1_r32]
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_close(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 3)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_shape_msg_))

    call check(pdict%items(2)%name == "shape1")
    call check(matches_type_value(pdict%items(2)%value, shape(value1, kind=i64)))

    call check(pdict%items(3)%name == "shape2")
    call check(matches_type_value(pdict%items(3)%value, shape(value2, kind=i64)))

  end subroutine test_close_r32r1_r32r1_mismatch_shape


  subroutine test_close_r32r1_r32r1_mismatch_value()
    real(r32), parameter :: value1(2) = [0.1_r32, 2.2e-12_r32]
    real(r32), parameter :: value2(2) = value1 + [1.0e-5_r32, 5e-14_r32]
    integer(i64), parameter :: mismatchloc(1) = [2]
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_close(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 4)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_value_msg_real_))

    call check(pdict%items(2)%name == "location")
    call check(matches_type_value(pdict%items(2)%value, mismatchloc))

    call check(pdict%items(3)%name == "value1")
    call check(matches_type_value(pdict%items(3)%value, value1(mismatchloc(1)), rtol=1e-6_r32,&
        & atol=1e-6_r32))

    call check(pdict%items(4)%name == "value2")
    call check(matches_type_value(pdict%items(4)%value, value2(mismatchloc(1)), rtol=1e-6_r32,&
        & atol=1e-6_r32))

  end subroutine test_close_r32r1_r32r1_mismatch_value


  subroutine test_close_r32r2_r32r2_success()
    real(r32), parameter :: value1(1, 2) = reshape([-5.0e20_r32, 128.0_r32], [1, 2])
    real(r32), parameter :: value2(1, 2) = value1 + reshape([1e-25_r32, 0.002_r32], [1, 2])
    type(check_result) :: res

    res = all_close(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_close_r32r2_r32r2_success


  subroutine test_close_r32r2_r32r2_mismatch_shape()
    real(r32), parameter :: value1(1, 2) = reshape([-5.0e20_r32, 128.0_r32], [1, 2])
    real(r32), parameter :: value2(1, 1) = reshape([1.0_r32], [1, 1])
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_close(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 3)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_shape_msg_))

    call check(pdict%items(2)%name == "shape1")
    call check(matches_type_value(pdict%items(2)%value, shape(value1, kind=i64)))

    call check(pdict%items(3)%name == "shape2")
    call check(matches_type_value(pdict%items(3)%value, shape(value2, kind=i64)))

  end subroutine test_close_r32r2_r32r2_mismatch_shape


  subroutine test_close_r32r2_r32r2_mismatch_value()
    real(r32), parameter :: value1(1, 2) = reshape([-5.0e20_r32, 128.0_r32], [1, 2])
    real(r32), parameter :: value2(1, 2) = value1 + reshape([1e-25_r32, 0.02_r32], [1, 2])
    integer(i64), parameter :: mismatchloc(2) = [1, 2]
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_close(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 4)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_value_msg_real_))

    call check(pdict%items(2)%name == "location")
    call check(matches_type_value(pdict%items(2)%value, mismatchloc))

    call check(pdict%items(3)%name == "value1")
    call check(matches_type_value(pdict%items(3)%value, value1(mismatchloc(1), mismatchloc(2)),&
        & rtol=1e-6_r32, atol=1e-6_r32))

    call check(pdict%items(4)%name == "value2")
    call check(matches_type_value(pdict%items(4)%value, value2(mismatchloc(1), mismatchloc(2)),&
        & rtol=1e-6_r32, atol=1e-6_r32))

  end subroutine test_close_r32r2_r32r2_mismatch_value


  subroutine test_close_r32r1_r32r0_success()
    real(r32), parameter :: value1(2) = [0.1_r32, 0.100001_r32]
    real(r32), parameter :: value2 = 0.1_r32
    type(check_result) :: res

    res = all_close(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_close_r32r1_r32r0_success


  subroutine test_close_r32r1_r32r0_mismatch()
    real(r32), parameter :: value1(2) = [0.1_r32, 2.2e-12_r32]
    real(r32), parameter :: value2 = 0.100001_r32
    integer(i64), parameter :: mismatchloc(1) = [2]
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_close(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 4)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_value_msg_real_))

    call check(pdict%items(2)%name == "location")
    call check(matches_type_value(pdict%items(2)%value, mismatchloc))

    call check(pdict%items(3)%name == "value1")
    call check(matches_type_value(pdict%items(3)%value, value1(mismatchloc(1)), rtol=1e-6_r32,&
        & atol=1e-6_r32))

    call check(pdict%items(4)%name == "value2")
    call check(matches_type_value(pdict%items(4)%value, value2, rtol=1e-6_r32, atol=1e-6_r32))

  end subroutine test_close_r32r1_r32r0_mismatch


  subroutine test_close_r32r0_r32r1_success()
    real(r32), parameter :: value1 = 0.1_r32
    real(r32), parameter :: value2(2) = [0.1_r32, 0.100001_r32]
    type(check_result) :: res

    res = all_close(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_close_r32r0_r32r1_success


  subroutine test_close_r32r0_r32r1_mismatch()
    real(r32), parameter :: value1 = 0.100001_r32
    real(r32), parameter :: value2(2) = [0.1_r32, 2.2e-12_r32]
    integer(i64), parameter :: mismatchloc(1) = [2]
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_close(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 4)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_value_msg_real_))

    call check(pdict%items(2)%name == "location")
    call check(matches_type_value(pdict%items(2)%value, mismatchloc))

    call check(pdict%items(3)%name == "value1")
    call check(matches_type_value(pdict%items(3)%value, value1, rtol=1e-6_r32, atol=1e-6_r32))

    call check(pdict%items(4)%name == "value2")
    call check(matches_type_value(pdict%items(4)%value, value2(mismatchloc(1)), rtol=1e-6_r32,&
        & atol=1e-6_r32))

  end subroutine test_close_r32r0_r32r1_mismatch


  subroutine test_close_r32r2_r32r0_success()
    real(r32), parameter :: value1(2, 1) = reshape([0.1_r32, 0.100001_r32], [2, 1])
    real(r32), parameter :: value2 = 0.1_r32
    type(check_result) :: res

    res = all_close(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_close_r32r2_r32r0_success


  subroutine test_close_r32r2_r32r0_mismatch()
    real(r32), parameter :: value1(2, 1) = reshape([0.1_r32, 2.2e-12_r32], [2, 1])
    real(r32), parameter :: value2 = 0.100001_r32
    integer(i64), parameter :: mismatchloc(2) = [2, 1]
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_close(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 4)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_value_msg_real_))

    call check(pdict%items(2)%name == "location")
    call check(matches_type_value(pdict%items(2)%value, mismatchloc))

    call check(pdict%items(3)%name == "value1")
    call check(matches_type_value(pdict%items(3)%value, value1(mismatchloc(1), mismatchloc(2)),&
        & rtol=1e-6_r32, atol=1e-6_r32))

    call check(pdict%items(4)%name == "value2")
    call check(matches_type_value(pdict%items(4)%value, value2, rtol=1e-6_r32, atol=1e-6_r32))

  end subroutine test_close_r32r2_r32r0_mismatch


  subroutine test_close_r32r0_r32r2_success()
    real(r32), parameter :: value1 = 0.1_r32
    real(r32), parameter :: value2(2, 1) = reshape([0.1_r32, 0.100001_r32], [2, 1])
    type(check_result) :: res

    res = all_close(value1, value2)
    call check(res%success)
    call check(.not. allocated(res%details))

  end subroutine test_close_r32r0_r32r2_success


  subroutine test_close_r32r0_r32r2_mismatch()
    real(r32), parameter :: value1 = 0.100001_r32
    real(r32), parameter :: value2(2, 1) = reshape([0.1_r32, 2.2e-12_r32], [2, 1])
    integer(i64), parameter :: mismatchloc(2) = [2, 1]
    type(check_result), target :: res
    type(details_dict), pointer :: pdict

    res = all_close(value1, value2)

    call check(.not. res%success)
    call check(allocated(res%details))
    if (failed()) return

    call get_ptr_to(res%details, pdict)
    call check(associated(pdict))
    if (failed()) return

    call check(size(pdict%items) == 4)
    if (failed()) return

    call check(pdict%items(1)%name == "failure")
    call check(matches_type_value(pdict%items(1)%value, mismatch_value_msg_real_))

    call check(pdict%items(2)%name == "location")
    call check(matches_type_value(pdict%items(2)%value, mismatchloc))

    call check(pdict%items(3)%name == "value1")
    call check(matches_type_value(pdict%items(3)%value, value1, rtol=1e-6_r32, atol=1e-6_r32))

    call check(pdict%items(4)%name == "value2")
    call check(matches_type_value(pdict%items(4)%value, value2(mismatchloc(1), mismatchloc(2)),&
        & rtol=1e-6_r32, atol=1e-6_r32))

  end subroutine test_close_r32r0_r32r2_mismatch


  function tests()
    type(test_list) :: tests

    tests = test_list([&
        suite("checkers", test_list([&
            suite("equal", test_list([&
                test("i32_i32_success", test_equal_i32_i32_success),&
                test("i32_i32_mismatch", test_equal_i32_i32_mismatch),&
                test("i32r1_i32r1_success", test_equal_i32r1_i32r1_success),&
                test("i32r1_i32r1_mismatch_shape", test_equal_i32r1_i32r1_mismatch_shape),&
                test("i32r1_i32r1_mismatch_value", test_equal_i32r1_i32r1_mismatch_value),&
                test("i32r2_i32r2_success", test_equal_i32r2_i32r2_success),&
                test("i32r2_i32r2_mismatch_shape", test_equal_i32r2_i32r2_mismatch_shape),&
                test("i32r2_i32r2_mismatch_value", test_equal_i32r2_i32r2_mismatch_value),&
                test("i32r1_i32r0_success", test_equal_i32r1_i32r0_success),&
                test("i32r1_i32r0_mismatch", test_equal_i32r1_i32r0_mismatch),&
                test("i32r0_i32r1_success", test_equal_i32r0_i32r1_success),&
                test("i32r0_i32r1_mismatch", test_equal_i32r0_i32r1_mismatch),&
                test("i32r2_i32r0_success", test_equal_i32r2_i32r0_success),&
                test("i32r2_i32r0_mismatch", test_equal_i32r2_i32r0_mismatch),&
                test("i32r0_i32r2_success", test_equal_i32r0_i32r2_success),&
                test("i32r0_i32r2_mismatch", test_equal_i32r0_i32r2_mismatch)&
            ])),&
            suite("close", test_list([&
                test("r32_r32_success", test_close_r32_r32_success),&
                test("r32_r32_mismatch", test_close_r32_r32_mismatch),&
                test("r32r1_r32r1_success", test_close_r32r1_r32r1_success),&
                test("r32r1_r32r1_mismatch_shape", test_close_r32r1_r32r1_mismatch_shape),&
                test("r32r1_r32r1_mismatch_value", test_close_r32r1_r32r1_mismatch_value),&
                test("r32r2_r32r2_success", test_close_r32r2_r32r2_success),&
                test("r32r2_r32r2_mismatch_shape", test_close_r32r2_r32r2_mismatch_shape),&
                test("r32r2_r32r2_mismatch_value", test_close_r32r2_r32r2_mismatch_value),&
                test("r32r1_r32r0_success", test_close_r32r1_r32r0_success),&
                test("r32r1_r32r0_mismatch", test_close_r32r1_r32r0_mismatch),&
                test("r32r0_r32r1_success", test_close_r32r0_r32r1_success),&
                test("r32r0_r32r1_mismatch", test_close_r32r0_r32r1_mismatch),&
                test("r32r2_r32r0_success", test_close_r32r2_r32r0_success),&
                test("r32r2_r32r0_mismatch", test_close_r32r2_r32r0_mismatch),&
                test("r32r0_r32r2_success", test_close_r32r0_r32r2_success),&
                test("r32r0_r32r2_mismatch", test_close_r32r0_r32r2_mismatch)&
            ]))&
        ]))&
    ])

  end function tests

end module test_checkers
