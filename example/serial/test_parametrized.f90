! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Demonstrates a possible realization of parametrized tests.
module test_parametrized
  use mylib, only : factorial
  use fortuno_serial, only : str, is_equal, serial_case_base, check => serial_check,&
      & suite => serial_suite_item, test_item, test_list
  implicit none

  private
  public :: tests


  !> Contains argument and expected result of a factorial() invokation
  type :: arg_res
    integer :: arg, res
  end type

  !> Argument/result pairs to check for
  type(arg_res), parameter :: testcaseparams(*) = [&
      & arg_res(1, 1), arg_res(2, 2), arg_res(3, 6), arg_res(4, 24), arg_res(5, 120)&
      & ]


  !> Parametrized test checking for an individual argument/result pair.
  type, extends(serial_case_base) :: parametrized_test_case
    type(arg_res) :: argres
  contains
    procedure :: run => parametrized_test_case_run
  end type parametrized_test_case

contains


  !> Returns the tests of this module.
  function tests()
    type(test_list) :: tests

    integer :: ii

    tests = test_list([&
        suite("parametrized", test_list([&
            (parametrized_test("factorial", testcaseparams(ii)), ii = 1, size(testcaseparams))&
        ]))&
    ])

  end function tests


  !> Convenience function to generate a parametrized test item (to be used in an array constructor)
  function parametrized_test(prefix, argres) result(testitem)
    character(*), intent(in) :: prefix
    type(arg_res), intent(in) :: argres
    type(test_item) :: testitem

    character(:), allocatable :: name

    name = prefix // "_" // str(argres%arg)
    testitem = test_item(parametrized_test_case(name=name, argres=argres))

  end function parametrized_test


  !> Run method of the parametrized test (executing the check directly)
  subroutine parametrized_test_case_run(this)
    class(parametrized_test_case), intent(in) :: this

    call check(is_equal(factorial(this%argres%arg), this%argres%res))

  end subroutine parametrized_test_case_run

end module test_parametrized
