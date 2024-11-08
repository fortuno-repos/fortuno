! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains some built-in checkers instantiations (32 bit real)
module fortuno_checkers_real32
  ! Template parameters
  use fortuno_env, only : rk => r32
  use fortuno_chartypes, only : value_item => real32_item
  ! Template dependencies
  use fortuno_env, only : i64
  use fortuno_checkfuncs, only : is_close_elem
  use fortuno_checkers_helpers, only : add_shape_mismatch_details, add_value_mismatch_details
  use fortuno_testinfo, only : check_result
  implicit none

  private
  public :: is_close, all_close

  !> Checks whether two entities are close each other
  interface is_close
    module procedure is_close_r0_r0
  end interface is_close

  !> Checks whether all entities in an array are close to each other
  interface all_close
    module procedure is_close_r0_r0
    module procedure all_close_r1_r1
    module procedure all_close_r2_r2
    module procedure all_close_r1_r0, all_close_r0_r1
    module procedure all_close_r2_r0, all_close_r0_r2
  end interface all_close

contains

  include 'real_template.inc'

end module fortuno_checkers_real32