! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains some supporting functions for checks.
module fortuno_checkfuncs
  use fortuno_env, only : r32, r64
  implicit none

  private
  public :: is_close_elem

  !> Whether two elements are close to each other
  interface is_close_elem
    module procedure is_close_elem_real32
    module procedure is_close_elem_real64
  end interface is_close_elem

contains


  elemental function is_close_elem_real32(value1, value2, rtol, atol) result(isclose)

    !> First value to check
    real(r32), intent(in) :: value1

    !> Second value to check
    real(r32), intent(in) :: value2

    !> Relative tolerance for the comparison (default: 1e-4)
    real(r32), optional, intent(in) :: rtol

    !> Absolute tolerance for the comparison (default: 0.0)
    real(r32), optional, intent(in) :: atol

    !> Whether the two values are close to each other
    logical :: isclose

    real(r32), parameter :: atoldef = 0.0_r32, rtoldef = 1e-4_r32
    real(r32) :: rtol_, atol_

    atol_ = atoldef
    if (present(atol)) atol_ = atol
    rtol_ = rtoldef
    if (present(rtol)) rtol_ = rtol

    isclose = abs(value1 - value2) <= max(rtol_ * max(abs(value1), abs(value2)), atol_)

  end function is_close_elem_real32


  elemental function is_close_elem_real64(value1, value2, rtol, atol) result(isclose)

    !> First value to check
    real(r64), intent(in) :: value1

    !> Second value to check
    real(r64), intent(in) :: value2

    !> Relative tolerance for the comparison (default: 1e-10)
    real(r64), optional, intent(in) :: rtol

    !> Absolute tolerance for the comparison (default: 0.0)
    real(r64), optional, intent(in) :: atol

    !> Whether the two values are close to each other
    logical :: isclose

    real(r64), parameter :: atoldef = 0.0_r64, rtoldef = 1e-10_r64
    real(r64) :: rtol_, atol_

    atol_ = atoldef
    if (present(atol)) atol_ = atol
    rtol_ = rtoldef
    if (present(rtol)) rtol_ = rtol

    isclose = abs(value1 - value2) <= max(rtol_ * max(abs(value1), abs(value2)), atol_)

  end function is_close_elem_real64

end module fortuno_checkfuncs
