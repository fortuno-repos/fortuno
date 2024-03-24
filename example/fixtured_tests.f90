! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

module fixtured_tests
  use mylib, only : factorial
  use fortuno_serial, only : char_rep_int, check => serial_check, is_equal, named_state,&
      & named_item, suite => serial_suite_item, store_state => serial_store_state,&
      & serial_case_base, test_item
  implicit none

  private
  public :: get_fixtured_tests


  ! Fixtured test case creating a random number before running a test procedure.
  type, extends(serial_case_base) :: random_test_case

  ! Test procedure to be invoked once fixture setup had been executed
    procedure(test_recursion_down), pointer, nopass :: proc

  contains

    ! Overrides run procedure to set up fixture before test procedure is invoked.
    procedure :: run => random_test_case_run

  end type random_test_case

contains


  ! Returns the tests from this module.
  function get_fixtured_tests() result(testitems)
    type(test_item), allocatable :: testitems(:)

    integer :: ii

    testitems = [&
        suite("fixtured", [&
            [(random_test("recursion_down", test_recursion_down), ii = 1, 5)],&
            [(random_test("recursion_up", test_recursion_up), ii = 1, 5)]&
        ])&
    ]

  end function get_fixtured_tests


  ! TEST n! = n * (n - 1)!
  subroutine test_recursion_down(nn)
    integer, intent(in) :: nn
    call check(is_equal(factorial(nn), nn * factorial(nn - 1)))
  end subroutine test_recursion_down


  ! TEST (n + 1)! = (n + 1) * n!
  subroutine test_recursion_up(nn)
    integer, intent(in) :: nn
    call check(is_equal(factorial(nn + 1), (nn  + 1) * factorial(nn)))
  end subroutine test_recursion_up


  ! Convenience function returning a random_test_case instance wrapped as test_item.
  function random_test(name, proc) result(testitem)
    character(*), intent(in) :: name
    procedure(test_recursion_down) :: proc
    type(test_item) :: testitem

    testitem%item = random_test_case(name=name, proc=proc)

  end function random_test


  ! Run procedure of the random_test_case class.
  subroutine random_test_case_run(this)
    class(random_test_case), intent(in) :: this

    real :: rand
    integer :: nn

    ! Set-up fixture by creating a random number
    call random_number(rand)
    ! Note: factorial(n) with n > 13 overflows with 32 bit integers
    nn = int(13 * rand) + 1
    ! Store internal state to aid introspection/identification later
    call store_state(&
        named_state([&
            named_item("n", char_rep_int(nn))&
        &])&
    )
    call this%proc(nn)

  end subroutine random_test_case_run

end module fixtured_tests
