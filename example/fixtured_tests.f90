! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

module fixtured_tests
  use mylib, only : factorial
  use fortuno_serial, only : check => serial_check, test => serial_case_item,&
      & suite => serial_suite_item, serial_case_base, test_item
  implicit none

  private
  public :: get_fixtured_tests


  ! Fixtured test case creating a random number before running a test procedure.
  type, extends(serial_case_base) :: random_test_case

    ! The random integer generated for a given test instance.
    integer :: nn = -1

    ! Test procedure to invoke once fixture had been created.
    procedure(test_recursion_down), pointer, nopass :: proc
  contains

    ! Overrides run procedure to execute fixture.
    procedure :: run => random_test_case_run

    !> Provides character representation of the internal state.
    procedure :: get_as_char => random_test_case_get_as_char
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
    call check(factorial(nn) == nn * factorial(nn - 1))
  end subroutine test_recursion_down


  ! TEST (n + 1)! = (n + 1) * n!
  subroutine test_recursion_up(nn)
    integer, intent(in) :: nn
    call check(factorial(nn + 1) == (nn  + 1) * factorial(nn))
    ! Creating a "random" error to demonstrate failure reporting with internal state
    call check(nn < 15)
  end subroutine test_recursion_up


  ! Returns a random_test_case instance wrapped as test_item.
  function random_test(name, proc) result(testitem)
    character(*), intent(in) :: name
    procedure(test_recursion_down) :: proc
    type(test_item) :: testitem

    testitem%item = random_test_case(name=name, proc=proc)

  end function random_test


  ! Runs procedure of the random_test_case class.
  subroutine random_test_case_run(this)
    class(random_test_case), intent(inout) :: this

    real :: rand

    ! Set-up fixture by creating a random number
    ! Number is stored as instance variable so that character representation of the internal state
    ! can be obtained after test had been executed.
    call random_number(rand)
    this%nn = int(20.0 * rand) + 1
    call this%proc(this%nn)

  end subroutine random_test_case_run


  ! Returns representation of the internal state of a random_test_case_instance.
  subroutine random_test_case_get_as_char(this, repr)
    class(random_test_case), intent(in) :: this
    character(:), allocatable, intent(out) :: repr

    character(5) :: buffer

    write(buffer, "(a3, i2.2)") "n: ", this%nn
    repr = buffer

  end subroutine random_test_case_get_as_char

end module fixtured_tests
