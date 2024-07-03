! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

module parametrized_tests
  use mylib, only : factorial
  use fortuno_serial, only : as_char, is_equal, serial_case_base, check => serial_check,&
      & suite => serial_suite_item, test_item
  implicit none

  private
  public :: parametrized_test_items


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
  function parametrized_test_items() result(testitems)
    type(test_item), allocatable :: testitems(:)

    integer :: ii

    testitems = [&
        suite("parametrized", [&
            (parametrized_test("factorial", testcaseparams(ii)), ii = 1, size(testcaseparams))&
        ])&
    ]

  end function parametrized_test_items


  !> Convenience wrapper to generate a test case for a given argres pair.
  function parametrized_test(prefix, argres) result(testitem)
    character(*), intent(in) :: prefix
    type(arg_res), intent(in) :: argres
    type(test_item) :: testitem

    character(:), allocatable :: name

    name = prefix // "_" // as_char(argres%arg)
    testitem%item = parametrized_test_case(name=name, argres=argres)

  end function parametrized_test


  !> Run method of the parametrized test (executing the check directly)
  subroutine parametrized_test_case_run(this)
    class(parametrized_test_case), intent(in) :: this

    call check(is_equal(factorial(this%argres%arg), this%argres%res))

  end subroutine parametrized_test_case_run

end module parametrized_tests
