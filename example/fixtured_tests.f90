! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

module fixtured_tests
  use mylib, only : factorial
  use fortuno, only : check => serial_check, test => serial_case_item,&
      & suite => serial_suite_item, test_item, test_ptr_item,&
      & serial_suite, serial_case_base
  implicit none

  private
  public :: get_fixtured_tests

  type, extends(serial_case_base) :: random_test_case
    integer :: nn = -1
    procedure(test_recursion_down), pointer, nopass :: proc
  contains
    procedure :: run => random_test_case_run
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


  subroutine test_recursion_down(nn)
    integer, intent(in) :: nn
    call check(factorial(nn) == nn * factorial(nn - 1))
  end subroutine test_recursion_down


  subroutine test_recursion_up(nn)
    integer, intent(in) :: nn
    call check(factorial(nn + 1) == (nn  + 1) * factorial(nn))
  end subroutine test_recursion_up



  function random_test(name, proc) result(testitem)
    character(*), intent(in) :: name
    procedure(test_recursion_down) :: proc
    type(test_item) :: testitem

    testitem%item = random_test_case(name=name, proc=proc)

  end function random_test


  subroutine random_test_case_run(this)
    class(random_test_case), intent(inout) :: this

    real :: rand

    call random_number(rand)
    this%nn = int(20.0 * rand) + 1
    call this%proc(this%nn)

  end subroutine random_test_case_run


  subroutine random_test_case_get_as_char(this, repr)
    class(random_test_case), intent(in) :: this
    character(:), allocatable, intent(out) :: repr

    character(4) :: buffer

    write(buffer, "(a2, i2.2)") "n=", this%nn
    repr = buffer

  end subroutine random_test_case_get_as_char

end module fixtured_tests
