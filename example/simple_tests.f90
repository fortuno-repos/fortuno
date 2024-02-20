! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

module simple_tests
  use mylib, only : factorial
  use fortuno, only : is_equal, test => serial_case_item, check => serial_check,&
      & suite => serial_suite_item, test_item
  implicit none

  private
  public :: get_simple_tests

contains

  ! Returns the tests from this module.
  function get_simple_tests() result(testitems)
    type(test_item), allocatable :: testitems(:)

    testitems = [&
        ! Adding a single test
        test("factorial_0", test_factorial_0),&

        ! Packing further tests into a suite in order to introduce more structure
        ! (e.g. running only tests being part of a given suite)
        suite("mysuite", [&
            test("factorial_1", test_factorial_1),&
            test("factorial_2", test_factorial_2)&
        ])&
    ]

  end function get_simple_tests

  ! Test: 0! = 1
  subroutine test_factorial_0()
    call check(factorial(0) == 1)
  end subroutine test_factorial_0

  ! Test: 1! = 1
  subroutine test_factorial_1()
    call check(factorial(1) == 1)
  end subroutine test_factorial_1

  ! Test: 2! = 2 (will fail to demonstrate the output of a failing test)
  subroutine test_factorial_2()
    ! Two failing checks, you should see info about both in the output
    call check(is_equal(factorial(2), 3),&
        & msg="Test failed for demonstration purposes",&
        & file="simple_tests.f90",&
        & line=43)
    call check(factorial(2) == 3)
  end subroutine test_factorial_2

end module simple_tests
