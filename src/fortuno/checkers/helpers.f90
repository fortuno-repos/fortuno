! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains some helper functions for the built-in checkers
module fortuno_checkers_helpers
  use fortuno_chartypes, only : stringable, details_dict, dict_item, int64_r1_item
  use fortuno_env, only : i64
  use fortuno_testinfo, only : check_result
  implicit none

  private
  public :: add_shape_mismatch_details, add_value_mismatch_details

contains


  subroutine add_shape_mismatch_details(shape1, shape2, checkresult)
    integer(i64), intent(in) :: shape1(:), shape2(:)
    type(check_result), intent(inout) :: checkresult

    ! Workaround:gfortran:14.1 (bug 116679)
    ! Omit array expression to avoid memory leak
    ! {-
    ! checkresult%details = details_dict([&
    !     & dict_item("failure", "mismatching array shapes"),&
    !     & dict_item("shape1", int64_r1_item(shape1)),&
    !     & dict_item("shape2", int64_r1_item(shape2))&
    !     & ])
    ! -}{+
    block
      type(details_dict), allocatable :: nameddetails
      allocate(nameddetails)
      allocate(nameddetails%items(3))
      associate (items => nameddetails%items)
          items(1) = dict_item("failure", "mismatching array shapes")
          items(2) = dict_item("shape1", int64_r1_item(shape1))
          items(3) = dict_item("shape2", int64_r1_item(shape2))
      end associate
      call move_alloc(nameddetails, checkresult%details)
    end block
    ! +}

  end subroutine add_shape_mismatch_details


  subroutine add_value_mismatch_details(msg, value1, value2, checkresult, mismatchloc)
    character(*), intent(in) :: msg
    class(stringable), intent(in) :: value1, value2
    type(check_result), intent(inout) :: checkresult
    integer(i64), optional, intent(in) :: mismatchloc(:)

    if (present(mismatchloc)) then
      ! Workaround:gfortran:14.1 (bug 116679)
      ! Omit array expression to avoid memory leak
      ! {-
      ! checkresult%details = details_dict([&
      !     & dict_item("failure", msg),&
      !     & dict_item("location", int64_r1_item(mismatchloc)),&
      !     & dict_item("value1", value1),&
      !     & dict_item("value2", value2)&
      !     & ])
      ! -}{+
      block
        type(details_dict), allocatable :: nameddetails
        allocate(nameddetails)
        allocate(nameddetails%items(4))
        associate (items => nameddetails%items)
            items(1) = dict_item("failure", msg)
            items(2) = dict_item("location", int64_r1_item(mismatchloc))
            items(3) = dict_item("value1", value1)
            items(4) = dict_item("value2", value2)
        end associate
        call move_alloc(nameddetails, checkresult%details)
      end block
      ! +}
    else
      ! Workaround:gfortran:14.1 (bug 116679)
      ! Omit array expression to avoid memory leak
      ! {-
      ! checkresult%details = details_dict([&
      !     & dict_item("failure", msg),&
      !     & dict_item("value1", value1),&
      !     & dict_item("value2", value2)&
      !     & ])
      ! -}{+
      block
        type(details_dict), allocatable :: nameddetails
        allocate(nameddetails)
        allocate(nameddetails%items(3))
        associate (items => nameddetails%items)
            items(1) = dict_item("failure", msg)
            items(2) = dict_item("value1", value1)
            items(3) = dict_item("value2", value2)
        end associate
        call move_alloc(nameddetails, checkresult%details)
      end block
      ! +}
    end if

  end subroutine add_value_mismatch_details

end module fortuno_checkers_helpers
