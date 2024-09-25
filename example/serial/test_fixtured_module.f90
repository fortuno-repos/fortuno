! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Demonstrates the realization of a global module wide fixture (not really recommended).
!!
!! Note: The test procedures in this example access the module variable containing the fixture data
!! directly via host association. This is a trade-off for more simplicity (in implementation) versus
!! less robustness. It must be ensured, that all tests accessing the module variable are within the
!! suite which sets up the global data. Additionally, none of the tests should the global module
!! variable to ensure the indepence of the tests. Latter could be automatically ensured by deriving
!! a new test case class from type(serial_case_base) which reads the module fixture and passes it to
!! test procedures as "intent(in)" argument(s). See the example on fixtured tests for more details.
!!
module test_fixtured_module
  use mylib, only : factorial
  use fortuno_serial, only : check => serial_check, is_equal, test => serial_case_item,&
      & serial_suite_base, test_item, test_list
  implicit none

  private
  public :: tests


  ! Test suite initializing the global module variables during its setup.
  type, extends(serial_suite_base) :: random_test_suite
  contains
    procedure :: set_up => random_test_suite_set_up
  end type random_test_suite


  ! Global module fixture variable
  integer :: nn = 0

contains


  ! Returns the tests from this module.
  function tests()
    type(test_list) :: tests

    tests = test_list([&
        random_suite("fixtured_module", test_list([&
            test("recursion_down", test_recursion_down),&
            test("recursion_up", test_recursion_up)&
        ]))&
    ])

  end function tests


  ! TEST n! = n * (n - 1)!
  !
  ! Note: uses the global module variable nn
  !
  subroutine test_recursion_down()
    call check(is_equal(factorial(nn), nn * factorial(nn - 1)))
  end subroutine test_recursion_down


  ! TEST (n + 1)! = (n + 1) * n!
  !
  ! Note: uses the global module variable nn
  !
  subroutine test_recursion_up()
    call check(is_equal(factorial(nn + 1), (nn  + 1) * factorial(nn)))
  end subroutine test_recursion_up


  ! Returns a random_test_suite instance wrapped as test_item to be used in array constructors.
  function random_suite(name, tests) result(testitem)
    character(*), intent(in) :: name
    type(test_list), intent(in) :: tests
    type(test_item) :: testitem

    testitem = test_item(random_test_suite(name=name, tests=tests))

  end function random_suite


  ! Initializes the test suite by generating and storing a random number in a module variable.
  subroutine random_test_suite_set_up(this)
    class(random_test_suite), intent(inout) :: this

    real :: rand

    call random_number(rand)

    ! Note: factorial(n) with n > 13 overflows with 32 bit integers
    nn = int(13 * rand) + 1

  end subroutine random_test_suite_set_up

end module test_fixtured_module
