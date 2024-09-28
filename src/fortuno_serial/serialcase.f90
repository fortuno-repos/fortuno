! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains the simplest possible parameterless serial test implementation
module fortuno_serial_serialcase
  use fortuno, only : test_item
  use fortuno_serial_serialbasetypes, only : serial_case_base
  implicit none

  private
  public :: serial_case, serial_case_item


  !> Serial test case with simple (parameterless) test procedure
  type, extends(serial_case_base) :: serial_case

    !> Test procedure to call when test is run
    procedure(serial_case_proc), nopass, pointer :: proc => null()

  contains

    !> Runs the test case
    procedure, public :: run => serial_case_run

  end type serial_case


  abstract interface

    !> Simple parameterless test procedure
    subroutine serial_case_proc()
    end subroutine serial_case_proc

  end interface

contains

  !> Creates a serial test case as a generic test item
  function serial_case_item(name, proc) result(testitem)
    character(len=*), intent(in) :: name
    procedure(serial_case_proc) :: proc
    type(test_item) :: testitem

    call testitem%init(serial_case(name=name, proc=proc))

  end function serial_case_item


  !> Runs the test case, by invoking the registered test procedure
  subroutine serial_case_run(this)

    !> Instance
    class(serial_case), intent(in) :: this

    call this%proc()

  end subroutine serial_case_run

end module fortuno_serial_serialcase
