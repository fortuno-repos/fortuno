! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

! This example illustrates how to use module variables as fixtures. Note that the test procedures
! access the module variable containing the fixture data directly via host association for
! simplicity. If any test modifies a module variable, the tests will no longer be independent. As an
! alternative, you could derive a new test case class by extending serial_case_base, where test
! procedures receive the fixture data as `intent(in)` arguments. For further details, refer to the
! example on fixtured tests.

module fixtured_module_tests
  use mylib, only : factorial
  use fortuno_serial, only : check => serial_check, is_equal, test => serial_case_item,&
      & serial_suite_base, test_item
  implicit none

  private
  public :: fixtured_module_test_items


  ! Test suite containing data initialized through the set_up() procedure
  type, extends(serial_suite_base) :: random_module_suite
  contains
    procedure :: set_up => random_module_suite_set_up
  end type random_module_suite


  ! Module variable containing the fixture data
  integer :: nn = 0

contains


  ! Returns the tests from this module.
  function fixtured_module_test_items() result(testitems)
    type(test_item), allocatable :: testitems(:)

    testitems = [&
        random_module("fixtured_module", [&
            test("recursion_down", test_recursion_down),&
            test("recursion_up", test_recursion_up)&
        ])&
    ]

  end function fixtured_module_test_items


  ! TEST n! = n * (n - 1)!
  subroutine test_recursion_down()
    call check(is_equal(factorial(nn), nn * factorial(nn - 1)))
  end subroutine test_recursion_down


  ! TEST (n + 1)! = (n + 1) * n!
  subroutine test_recursion_up()
    call check(is_equal(factorial(nn + 1), (nn  + 1) * factorial(nn)))
  end subroutine test_recursion_up


  ! Convenience function returning a random_module_suite instance wrapped as test_item.
  function random_module(name, items) result(testitem)
    character(*), intent(in) :: name
    type(test_item), intent(in) :: items(:)
    type(test_item) :: testitem

    testitem%item = random_module_suite(name=name, items=items)

  end function random_module


  ! Initializes the test suite by generating and storing a random number.
  subroutine random_module_suite_set_up(this)
    class(random_module_suite), intent(inout) :: this

    real :: rand

    call random_number(rand)
    ! Note: factorial(n) with n > 13 overflows with 32 bit integers
    nn = int(13 * rand) + 1

  end subroutine random_module_suite_set_up

end module fixtured_module_tests
