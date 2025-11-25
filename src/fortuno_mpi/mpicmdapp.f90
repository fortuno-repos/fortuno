! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains the command line app for driving mpi tests
module fortuno_mpi_mpicmdapp
  use fortuno, only : cmd_app, test_list
  use fortuno_mpi_mpidriver, only : init_mpi_driver, mpi_driver
  use fortuno_mpi_mpienv, only : init_mpi_env, final_mpi_env, mpi_env
  use fortuno_mpi_mpiconlogger, only : init_mpi_console_logger, mpi_console_logger
  implicit none

  private
  public :: init_mpi_cmd_app, execute_mpi_cmd_app, mpi_cmd_app


  !> App for driving mpi tests through command line app
  type, extends(cmd_app) :: mpi_cmd_app
  end type mpi_cmd_app

contains

  !> Convenience wrapper setting up and running the mpi command line up
  !!
  !! Note: This routine stops the code during execution and never returns.
  !!
  subroutine execute_mpi_cmd_app(tests, mpi_thread_level)

    !> Items to be considered by the app
    type(test_list), intent(in) :: tests

    !> MPI thread level
    integer, optional, intent(in) :: mpi_thread_level

    integer :: exitcode

    call run_mpi_cmd_app(tests, exitcode, mpi_thread_level)
    stop exitcode, quiet=.true.

  end subroutine execute_mpi_cmd_app


  !> Sets up and runs the mpi command line up
  subroutine run_mpi_cmd_app(tests, exitcode, mpi_thread_level)

    !> Items to be considered by the app
    type(test_list), intent(in) :: tests

    !> Exit code
    integer, intent(out) :: exitcode

    !> MPI thread level
    integer, optional, intent(in) :: mpi_thread_level

    type(mpi_cmd_app) :: app
    type(mpi_env) :: mpienv

    call init_mpi_env(mpienv, mpi_thread_level)
    call init_mpi_cmd_app(app, mpienv)
    call app%run(tests, exitcode)
    call final_mpi_env(mpienv)

  end subroutine run_mpi_cmd_app


  !> Set up the mpi command line app
  subroutine init_mpi_cmd_app(this, mpienv)

    !> Instance
    type(mpi_cmd_app), intent(out) :: this

    !> MPI environment
    type(mpi_env), intent(in) :: mpienv

    type(mpi_console_logger), allocatable :: logger
    type(mpi_driver), allocatable :: driver

    allocate(logger)
    call init_mpi_console_logger(logger, mpienv)
    call move_alloc(logger, this%logger)
    allocate(driver)
    call init_mpi_driver(driver, mpienv)
    call move_alloc(driver, this%driver)

  end subroutine init_mpi_cmd_app

end module fortuno_mpi_mpicmdapp
