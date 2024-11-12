! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Demonstrates the simplest possible testing scenario
module test_simple
  use mylib, only : cotan, factorial, r32
  use fortuno_serial, only : all_close, is_equal, test => serial_case_item, check => serial_check,&
      & suite => serial_suite_item, test_list
  implicit none

  private
  public :: tests

contains


  ! Returns the tests from this module.
  function tests()
    type(test_list) :: tests

    tests = test_list([&
        ! Best practice is to create at least one suite with the name of the module and put the
        ! tests in it, like below. You might further structure your test sets by nesting further
        ! suites into the top level one.
        suite("simple", test_list([&
            test("factorial_0", test_factorial_0),&
            test("factorial_1", test_factorial_1),&
            test("factorial_2", test_factorial_2),&
            test("cotan", test_cotan)&
        ]))&
    ])

  end function tests


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
    ! Both check will fail, you should see info about both in the output
    ! The file and line information are provided manually. Check the examples in the fpp example
    ! folders for automatic file name and line number generation.
    call check(is_equal(factorial(2), 2),&
        & msg="Test failed for demonstration purposes",&
        & file="test_simple.f90",&
        & line=51)
    call check(factorial(2) == 2)
  end subroutine test_factorial_2


  ! Test specific values for a rank-2 cotangent calculation.
  subroutine test_cotan()
    real(r32), parameter :: xvals(2, 2) = reshape(&
        & [1.47112767_r32, 1.23412151_r32, 1.03037683_r32, 0.86630226_r32], [2, 2])
    real(r32), parameter :: cotanvals(2, 2) = reshape(&
        & [0.1000000_r32, 0.3500000_r32, 0.6000000_r32, 0.8500000_r32], [2, 2])

    real(r32), allocatable :: yvals(:,:)

    yvals = cotanvals
    ! We add a "bug" for the 3rd element to demonstrate the failure
    yvals(1, 2) = yvals(1, 2) + 0.1_r32

    ! This will fail and print information about the first failing value pair and its position
    call check(all_close(cotan(xvals), yvals, rtol=1e-5, atol=0.0))

  end subroutine test_cotan

end module test_simple
