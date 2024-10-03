! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

#include "fortuno_serial.fpp"

!> Demo for realizing fixtured tests by overriding the run() method of the test_case object.
module test_fixtured_fpp
  use mylib, only : factorial
  use fortuno_serial, only : char_rep_int, is_equal, named_state, named_item,&
      & suite => serial_suite_item, store_state => serial_store_state, serial_case_base, test_item,&
      & test_list
  implicit none

  private
  public :: tests


  ! Fixtured test case creating a random number before running a test procedure.
  type, extends(serial_case_base) :: random_test_case

  ! Test procedure to be called after fixture setup had finished.
    procedure(test_recursion_down), pointer, nopass :: proc

  contains

    ! Overrides run procedure to set up fixture before test procedure is invoked.
    procedure :: run => random_test_case_run

  end type random_test_case

contains


  ! Returns the tests from this module.
  function tests()
    type(test_list) :: tests

    integer :: ii

    tests = test_list([&
        suite("fixtured", test_list([&
            [(random_test("recursion_down", test_recursion_down), ii = 1, 5)],&
            [(random_test("recursion_up", test_recursion_up), ii = 1, 5)]&
        ]))&
    ])

  end function tests


  ! TEST n! = n * (n - 1)!
  subroutine test_recursion_down(nn)
    integer, intent(in) :: nn
    CHECK(is_equal(factorial(nn), nn * factorial(nn - 1)))
  end subroutine test_recursion_down


  ! TEST (n + 1)! = (n + 1) * n!
  subroutine test_recursion_up(nn)
    integer, intent(in) :: nn
    CHECK(is_equal(factorial(nn + 1), (nn  + 1) * factorial(nn)))
  end subroutine test_recursion_up


  ! Returns a random_test_case instance wrapped as test_item to be used in an array constructor.
  function random_test(name, proc) result(testitem)
    character(*), intent(in) :: name
    procedure(test_recursion_down) :: proc
    type(test_item) :: testitem

    testitem = test_item(random_test_case(name=name, proc=proc))

  end function random_test


  ! Runs procedure of the random_test_case class.
  subroutine random_test_case_run(this)
    class(random_test_case), intent(in) :: this

    real :: rand
    integer :: nn

    ! Set-up fixture by creating a random number
    call random_number(rand)

    ! Note: factorial(n) with n > 13 overflows with 32 bit integers
    nn = int(13 * rand) + 1

    ! Store internal state (actual value of nn) to aid introspection/identification later
    ! Workaround:gfortran:14.1 (bug 116679)
    ! Omit array expression to avoid memory leak
    ! {-
    ! call store_state(&
    !     named_state([&
    !         named_item("n", char_rep_int(nn))&
    !     &])&
    ! )
    ! -}{+
    block
      type(named_item) :: nameditems(1)
      nameditems(1) = named_item("n", char_rep_int(nn))
      call store_state(named_state(nameditems))
    end block
    call this%proc(nn)
    ! +}

  end subroutine random_test_case_run

end module test_fixtured_fpp
