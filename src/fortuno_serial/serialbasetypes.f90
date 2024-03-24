! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains base type for serial test suites
module fortuno_serial_serialbasetypes
  use fortuno, only : test_case_base, test_suite_base
  implicit none

  private
  public :: serial_case_base, serial_case_base_run, as_serial_case_base
  public :: serial_suite_base, as_serial_suite_base


  !> Base class for all serial test cases
  type, extends(test_case_base), abstract :: serial_case_base
  contains
    !> Procedure to be invoked by the driver to run the test
    procedure(serial_case_base_run), deferred :: run
  end type serial_case_base


  abstract interface

    subroutine serial_case_base_run(this)
      import serial_case_base
      implicit none
      class(serial_case_base), intent(in) :: this
    end subroutine serial_case_base_run

  end interface


  !> Base type for all serial test suites
  type, extends(test_suite_base), abstract :: serial_suite_base
  contains
    procedure :: set_up => serial_suite_base_set_up
    procedure :: tear_down => serial_suite_base_tear_down
  end type serial_suite_base

contains

  !> Returns a serial_context class pointer to a generic context pointer
  function as_serial_case_base(trg) result(ptr)

    !> Target to point at
    class(test_case_base), pointer, intent(in) :: trg

    !> Class specific pointer
    class(serial_case_base), pointer :: ptr

    select type (trg)
    class is (serial_case_base)
      ptr => trg
    class default
      error stop "Invalid test context type obtained in as_serial_case_base"
    end select

  end function as_serial_case_base


  !> Returns a serial_context class pointer to a generic context pointer
  function as_serial_suite_base(trg) result(ptr)

    !> Target to point at
    class(test_suite_base), pointer, intent(in) :: trg

    !> Class specific pointer
    class(serial_suite_base), pointer :: ptr

    select type (trg)
    class is (serial_suite_base)
      ptr => trg
    class default
      error stop "Invalid test context type obtained in as_serial_suite_base"
    end select

  end function as_serial_suite_base


  !> Sets up the test suite, before the first test in the suite is run
  subroutine serial_suite_base_set_up(this)

    !> Instance
    class(serial_suite_base), intent(inout) :: this

  end subroutine serial_suite_base_set_up


  !> Tears downs the test suite, after the last test had been run
  subroutine serial_suite_base_tear_down(this)

    !> Instance
    class(serial_suite_base), intent(inout) :: this

  end subroutine serial_suite_base_tear_down

end module fortuno_serial_serialbasetypes
