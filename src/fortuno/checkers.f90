! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains some built-in checkers
module fortuno_checkers
  use fortuno_chartypes, only : char_rep_int, char_rep_int_r1, named_details, named_item
  use fortuno_testinfo, only : check_result
  implicit none

  private
  public :: is_equal
  public :: int_r1_item


  !> Checks whether two entities are equal
  interface is_equal
    module procedure is_equal_i0_i0, is_equal_i1_i1, is_equal_i2_i2
  end interface is_equal

  !> Item wrapper for integer rank 1 arrays
  type :: int_r1_item
    integer, allocatable :: value(:)
  end type int_r1_item

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
          & named_item("failure", "mismatching values"),&
          & named_item("value1", char_rep_int(value1)),&
          & named_item("value2", char_rep_int(value2))&
          & ])
    end if

  end function is_equal_i0_i0


  !> Checks whether two integer rank 1 arrays are equal.
  function is_equal_i1_i1(value1, value2) result(checkresult)

    !> First value to check
    integer, intent(in) :: value1(:)

    !> Second value to check
    integer, intent(in) :: value2(:)

    !> Result of the check
    type(check_result) :: checkresult

    logical, allocatable :: mismatch(:)
    integer :: mismatchloc(1)

    if (any(shape(value1) /= shape(value2))) then
      checkresult%success = .false.
      checkresult%details = named_details([&
          & named_item("failure", "mismatching array shapes"),&
          & named_item("shape1", char_rep_int_r1(shape(value1))),&
          & named_item("shape2", char_rep_int_r1(shape(value2)))&
          & ])
      return
    end if
    mismatch = value1 /= value2
    mismatchloc = findloc(mismatch, .true.)
    if (all(mismatchloc /= 0)) then
      checkresult%success = .false.
      checkresult%details = named_details([&
          & named_item("failure", "mismatching array elements"),&
          & named_item("location", char_rep_int_r1(mismatchloc)),&
          & named_item("value1", char_rep_int(value1(mismatchloc(1)))),&
          & named_item("value2", char_rep_int(value2(mismatchloc(1))))&
          & ])
      return
    end if
    checkresult%success = .true.

  end function is_equal_i1_i1


  !> Checks whether two integer rank 1 arrays are equal.
  function is_equal_i2_i2(value1, value2) result(checkresult)

    !> First value to check
    integer, intent(in) :: value1(:,:)

    !> Second value to check
    integer, intent(in) :: value2(:,:)

    !> Result of the check
    type(check_result) :: checkresult

    logical, allocatable :: mismatch(:,:)
    integer :: mismatchloc(2)

    if (any(shape(value1) /= shape(value2))) then
      checkresult%success = .false.
      checkresult%details = named_details([&
          & named_item("failure", "mismatching array shapes"),&
          & named_item("shape1", char_rep_int_r1(shape(value1))),&
          & named_item("shape2", char_rep_int_r1(shape(value2)))&
          & ])
      return
    end if
    mismatch = value1 /= value2
    mismatchloc = findloc(mismatch, .true.)
    if (all(mismatchloc /= 0)) then
      checkresult%success = .false.
      checkresult%details = named_details([&
          & named_item("failure", "mismatching array elements"),&
          & named_item("location", char_rep_int_r1(mismatchloc)),&
          & named_item("value1", char_rep_int(value1(mismatchloc(1), mismatchloc(2)))),&
          & named_item("value2", char_rep_int(value2(mismatchloc(1), mismatchloc(2))))&
          & ])
      return
    end if
    checkresult%success = .true.

  end function is_equal_i2_i2

end module fortuno_checkers