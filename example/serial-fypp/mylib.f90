! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Demo module/library to be tested
module mylib_fypp
  implicit none

  private
  public :: factorial

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

end module mylib_fypp
