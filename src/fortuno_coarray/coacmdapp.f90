! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains the command line app for driving coa tests
module fortuno_coarray_coacmdapp
  use fortuno, only : cmd_app, test_list
  use fortuno_coarray_coadriver, only : init_coa_driver, coa_driver
  use fortuno_coarray_coaenv, only : init_coa_env, final_coa_env, coa_env
  use fortuno_coarray_coaconlogger, only : init_coa_console_logger, coa_console_logger
  implicit none

  private
  public :: init_coa_cmd_app, execute_coa_cmd_app, coa_cmd_app


  !> App for driving coarray tests through command line app
  type, extends(cmd_app) :: coa_cmd_app
  end type coa_cmd_app

contains

  !> Convenience wrapper setting up and running the coarray command line app
  !!
  !! Note: This routine stops the code during execution and never returns.
  !!
  subroutine execute_coa_cmd_app(tests)

    !> Items to be considered by the app
    type(test_list), intent(in) :: tests

    integer :: exitcode

    call run_coa_cmd_app(tests, exitcode)
    stop exitcode, quiet=.true.

  end subroutine execute_coa_cmd_app


  !> Sets up and runs the coarray command line app
  subroutine run_coa_cmd_app(tests, exitcode)

    !> Items to be considered by the app
    type(test_list), intent(in) :: tests

    !> Exit code
    integer, intent(out) :: exitcode

    type(coa_cmd_app) :: app
    type(coa_env) :: coaenv

    call init_coa_env(coaenv)
    call init_coa_cmd_app(app, coaenv)
    call app%run(tests, exitcode)
    call final_coa_env(coaenv)

  end subroutine run_coa_cmd_app


  !> Set up the coa command line app
  subroutine init_coa_cmd_app(this, coaenv)

    !> Instance
    type(coa_cmd_app), intent(out) :: this

    !> coa environment
    type(coa_env), intent(in) :: coaenv

    type(coa_console_logger), allocatable :: logger
    type(coa_driver), allocatable :: driver

    allocate(logger)
    call init_coa_console_logger(logger, coaenv)
    call move_alloc(logger, this%logger)
    allocate(driver)
    call init_coa_driver(driver, coaenv)
    call move_alloc(driver, this%driver)

  end subroutine init_coa_cmd_app

end module fortuno_coarray_coacmdapp
