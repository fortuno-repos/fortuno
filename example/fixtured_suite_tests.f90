! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

module fixtured_suite_tests
  use mylib, only : factorial
  use fortuno_serial, only : char_rep_int, check => serial_check, is_equal, named_state,&
      & named_item, serial_case_base, scope_pointers => serial_scope_pointers,&
      & store_state => serial_store_state, serial_suite_base, test_item, test_ptr_item
  implicit none

  private
  public :: get_fixtured_suite_tests


  ! Test suite containing data initialized through the set_up() procedures
  type, extends(serial_suite_base) :: random_test_suite
    integer :: nn = -1
  contains
    procedure :: set_up => random_test_suite_set_up
  end type random_test_suite


  ! Test case reading the test suite's data during testing
  type, extends(serial_case_base) :: random_test_case

    ! Test procedure to be invoked for testing
    procedure(test_recursion_down), pointer, nopass :: proc

  contains

    ! Overrides run procedure to pass hosting suite's to test procedure
    procedure :: run => random_test_case_run

  end type random_test_case

contains


  ! Returns the tests from this module.
  function get_fixtured_suite_tests() result(testitems)
    type(test_item), allocatable :: testitems(:)

    testitems = [&
        random_suite("fixtured_suite", [&
          random_test("recursion_down", test_recursion_down),&
          random_test("recursion_up", test_recursion_up)&
        ])&
    ]

  end function get_fixtured_suite_tests


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


  ! Convenience function returning a random_test_suite instance wrapped as test_item.
  function random_suite(name, items) result(testitem)
    character(*), intent(in) :: name
    type(test_item), intent(in) :: items(:)
    type(test_item) :: testitem

    testitem%item = random_test_suite(name=name, items=items)

  end function random_suite


  ! Initializes the test suite by generating and storing a random number.
  subroutine random_test_suite_set_up(this)
    class(random_test_suite), intent(inout) :: this

    real :: rand

    call random_number(rand)
    ! Note: factorial(n) with n > 13 overflows with 32 bit integers
    this%nn = int(13 * rand) + 1

    ! Store internal state to aid introspection/identification later
    call store_state(&
        named_state([&
            named_item("n", char_rep_int(this%nn))&
        &])&
    )

  end subroutine random_test_suite_set_up


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

    type(test_ptr_item), allocatable :: scopeptrs(:)
    integer :: nn

    nn = -1
    scopeptrs = scope_pointers()
    ! scopeptrs(1): current scope - random_test_case instance
    ! scopeptrs(2): first enclosing scope - random_test_suite instance
    if (size(scopeptrs) < 2)&
        & error stop "fixtured_suite_tests::random_test_case_run: Size of scopeptrs too small"
    select type (suite => scopeptrs(2)%item)
    type is (random_test_suite)
      nn = suite%nn
    class default
      error stop "fixtured_suite_tests::random_test_case_run: Expected random_test_suite instance,&
          & obtained something else"
    end select

    call this%proc(nn)

  end subroutine random_test_case_run

end module fixtured_suite_tests
