! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

#include "fortuno_serial.fpp"

!> Demonstrates the simplest possible testing scenario
module test_simple_fpp
  use mylib, only : factorial
  use fortuno_serial, only : is_equal, test => serial_case_item, suite => serial_suite_item,&
      & test_list
  implicit none

  private
  public :: tests

contains


  ! Returns the tests from this module.
  function tests()
    type(test_list) :: tests

    tests = test_list([&
        test("factorial_0", test_factorial_0),&
        suite("simple", test_list([&
            test("factorial_1", test_factorial_1),&
            test("factorial_2", test_factorial_2)&
        ]))&
    ])

  end function tests

  ! Test: 0! = 1
  subroutine test_factorial_0()
    CHECK(factorial(0) == 1)
  end subroutine test_factorial_0

  ! Test: 1! = 1
  subroutine test_factorial_1()
    CHECK(factorial(1) == 1)
  end subroutine test_factorial_1

  ! Test: 2! = 2 (will fail due to the bug in the implementation of the factorial function)
  subroutine test_factorial_2()
    ! Two failing checks, you should see info about both in the output
    CHECK_MSG(is_equal(factorial(2), 2), "Test failed for demonstration purposes")
    CHECK(factorial(2) == 2)
  end subroutine test_factorial_2

end module test_simple_fpp
