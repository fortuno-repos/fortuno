! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains the command line app for driving serial tests
module fortuno_serial_serialcmdapp
  use fortuno, only : cmd_app, test_list
  use fortuno_serial_serialdriver, only : init_serial_driver, serial_driver
  use fortuno_serial_serialconlogger, only : serial_console_logger
  implicit none

  private
  public :: init_serial_cmd_app, serial_cmd_app
  public :: execute_serial_cmd_app, run_serial_cmd_app


  !> App for driving serial tests through command line app
  type, extends(cmd_app) :: serial_cmd_app
  end type serial_cmd_app

contains


  !> Executes the serial command line app
  !!
  !! Note: This routine stops the code during execution and never returns.
  !!
  subroutine execute_serial_cmd_app(tests)

    !> Items to be considered by the app
    type(test_list), intent(in) :: tests

    integer :: exitcode

    call run_serial_cmd_app(tests, exitcode)
    stop exitcode, quiet=.true.

  end subroutine execute_serial_cmd_app


  !> Convenience wrapper setting up and running the serial command line app.
  subroutine run_serial_cmd_app(tests, exitcode)

    !> Items to be considered by the app
    type(test_list), intent(in) :: tests

    !> Exit code the run should generate
    integer, intent(out) :: exitcode

    type(serial_cmd_app) :: app

    call init_serial_cmd_app(app)
    call app%run(tests, exitcode)

  end subroutine run_serial_cmd_app


  !> Set up the serial command line app
  subroutine init_serial_cmd_app(this)

    !> Instance
    type(serial_cmd_app), intent(out) :: this

    type(serial_driver), allocatable :: driver

    allocate(serial_console_logger :: this%logger)
    allocate(driver)
    call init_serial_driver(driver)
    call move_alloc(driver, this%driver)

  end subroutine init_serial_cmd_app

end module fortuno_serial_serialcmdapp
