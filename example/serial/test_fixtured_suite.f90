! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Demo for realizing fixtured suites providing common data for all tests within the suite.
module test_fixtured_suite
  use mylib, only : factorial
  use fortuno_serial, only : check => serial_check, is_equal, state_dict,&
      & dict_item, serial_case_base, scope_pointers => serial_scope_pointers,&
      & store_state => serial_store_state, serial_suite_base, test_item, test_list, test_ptr_item
  implicit none

  private
  public :: tests


  ! Test suite containing data initialized through the set_up() procedure
  type, extends(serial_suite_base) :: random_test_suite
    integer :: nn = -1
  contains
    procedure :: set_up => random_test_suite_set_up
  end type random_test_suite


  ! Customized test case reading the suite's data and passing it to the test procedure.
  type, extends(serial_case_base) :: random_test_case

    ! Test procedure to be invoked for testing
    procedure(test_recursion_down), pointer, nopass :: proc

  contains

    ! Overrides run procedure to pass hosting suite's data to test procedure
    procedure :: run => random_test_case_run

  end type random_test_case

contains


  ! Returns the tests from this module.
  function tests()
    type(test_list) :: tests

    tests = test_list([&
        random_suite("fixtured_suite", test_list([&
          random_test("recursion_down", test_recursion_down),&
          random_test("recursion_up", test_recursion_up)&
        ]))&
    ])

  end function tests


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


  ! Returns a random_test_suite instance wrapped as test_item to be used in array constructors.
  function random_suite(name, tests) result(testitem)
    character(*), intent(in) :: name
    type(test_list), intent(in) :: tests
    type(test_item) :: testitem

    testitem = test_item(random_test_suite(name=name, tests=tests))

  end function random_suite


  ! Initializes the test suite by generating and storing a random number.
  subroutine random_test_suite_set_up(this)
    class(random_test_suite), intent(inout) :: this

    real :: rand

    call random_number(rand)

    ! Note: factorial(n) with n > 13 overflows with 32 bit integers
    this%nn = int(13 * rand) + 1

    ! Store internal state to aid introspection/identification later
    ! Workaround:gfortran:14.1 (bug 116679)
    ! Omit array expression to avoid memory leak
    ! {-
    ! call store_state(&
    !     state_dict([&
    !         dict_item("n", this%nn)&
    !     &])&
    ! )
    ! -}{+
    block
      type(dict_item) :: dictitems(1)
      dictitems(1) = dict_item("n", this%nn)
      call store_state(state_dict(dictitems))
    end block
    ! +}

  end subroutine random_test_suite_set_up


  ! Returns a random_test_case instance wrapped as test_item to be used in array constructors.
  function random_test(name, proc) result(testitem)
    character(*), intent(in) :: name
    procedure(test_recursion_down) :: proc
    type(test_item) :: testitem

    testitem = test_item(random_test_case(name=name, proc=proc))

  end function random_test


  ! Customize run procedure of the random_test_case class reading the hosting suite's data.
  subroutine random_test_case_run(this)
    class(random_test_case), intent(in) :: this

    type(test_ptr_item), allocatable :: scopeptrs(:)
    integer :: nn

    nn = -1
    allocate(scopeptrs, source=scope_pointers())
    ! scopeptrs(1): current scope - random_test_case instance
    ! scopeptrs(2): first enclosing scope - random_test_suite instance
    if (size(scopeptrs) < 2)&
        & error stop "test_fixtured_suite::random_test_case_run: Size of scopeptrs too small"
    select type (suite => scopeptrs(2)%item)
    type is (random_test_suite)
      nn = suite%nn
    class default
      error stop "test_fixtured_suite::random_test_case_run: Expected random_test_suite instance,&
          & obtained something else"
    end select

    call this%proc(nn)

  end subroutine random_test_case_run

end module test_fixtured_suite
