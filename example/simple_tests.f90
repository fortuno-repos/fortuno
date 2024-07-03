! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

module simple_tests
  use mylib, only : factorial
  use fortuno_serial, only : is_equal, test => serial_case_item, check => serial_check,&
      & suite => serial_suite_item, test_item
  implicit none

  private
  public :: simple_test_items

contains

  ! Returns the tests from this module.
  function simple_test_items() result(testitems)
    type(test_item), allocatable :: testitems(:)

    testitems = [&
        ! Adding a single test not belonging to any test suite
        test("factorial_0", test_factorial_0),&

        ! Packing further tests into a suite in order to introduce more structure
        ! (e.g. running only tests being part of a given suite)
        suite("simple", [&
            test("factorial_1", test_factorial_1),&
            test("factorial_2", test_factorial_2)&
        ])&
    ]

  end function simple_test_items

  ! Test: 0! = 1
  subroutine test_factorial_0()
    call check(factorial(0) == 1)
  end subroutine test_factorial_0

  ! Test: 1! = 1
  subroutine test_factorial_1()
    call check(factorial(1) == 1)
  end subroutine test_factorial_1

  ! Test: 2! = 2 (will fail due to the bug in the implementation of the factorial function)
  subroutine test_factorial_2()
    ! Two failing checks, you should see info about both in the output
    call check(is_equal(factorial(2), 2),&
        & msg="Test failed for demonstration purposes",&
        & file="simple_tests.f90",&
        & line=45)
    call check(factorial(2) == 2)
  end subroutine test_factorial_2

end module simple_tests
