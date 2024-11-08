! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains some built-in checkers instantiations (32 bit integer)
module fortuno_checkers_int32
  ! Template parameters
  use fortuno_env, only : ik => i32
  use fortuno_chartypes, only : value_item => int32_item
  ! Template dependencies
  use fortuno_env, only : i64
  use fortuno_checkers_helpers, only : add_shape_mismatch_details, add_value_mismatch_details
  use fortuno_testinfo, only : check_result
  implicit none

  private
  public :: is_equal, all_equal


  !> Checks whether two entities are equal
  interface is_equal
    module procedure is_equal_r0_r0
  end interface is_equal

  !> Checks whether all entities in an array are equal
  interface all_equal
    module procedure is_equal_r0_r0
    module procedure all_equal_r1_r1
    module procedure all_equal_r2_r2
    module procedure all_equal_r1_r0, all_equal_r0_r1
    module procedure all_equal_r2_r0, all_equal_r0_r2
  end interface all_equal

contains

  include 'int_template.inc'

end module fortuno_checkers_int32