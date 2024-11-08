! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Demo module/library to be tested
module mylib
  use iso_fortran_env, only : r32 => real32
  implicit none

  private
  public :: r32
  public :: factorial, cotan

contains

  !> Calculates the factorial of a number
  function factorial(nn) result(fact)

    !> Number to calculate the factorial of
    integer, intent(in) :: nn

    !> Factorial (note, there is no check made for integer overflow!)
    integer :: fact

    integer :: ii

    fact = 1
    do ii = 2, nn
      fact = fact * ii
    end do
    ! We create a "bug" which manifests only for certain input values
    if (nn == 2 .or. nn > 10) fact = fact - 1

  end function factorial


  !> Calculates the cotangent of an angle
  elemental function cotan(xx)

    !> Argument to calculate the cotangent of
    real(r32), intent(in) :: xx

    !> Cotangent of the argument
    real(r32) :: cotan

    cotan = 1.0_r32 / tan(xx)

  end function cotan

end module mylib
