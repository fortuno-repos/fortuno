! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Implements a driver for mpi tests
module fortuno_mpi_mpidriver
  use fortuno, only : init_test_driver, test_case_base, test_context, test_driver, test_suite_base,&
      & test_runner
  use fortuno_mpi_mpibasetypes, only : as_mpi_case_base, as_mpi_suite_base,&
      & mpi_case_base, mpi_suite_base
  use fortuno_mpi_mpicontext, only : as_mpi_context, init_mpi_context_factory, mpi_context,&
      & mpi_context_factory
  use fortuno_mpi_mpienv, only : mpi_env
  use fortuno_mpi_mpiglobalctx, only : set_mpi_global_context
  implicit none

  private
  public :: init_mpi_driver, mpi_driver


  !> Runner implementation for mpi tests and test suites
  type, extends(test_runner) :: mpi_runner
  contains
    procedure :: set_up_suite => mpi_runner_set_up_suite
    procedure :: tear_down_suite => mpi_runner_tear_down_suite
    procedure :: run_test => mpi_runner_run_test
  end type mpi_runner


  !> Driver for mpi tests
  type, extends(test_driver) :: mpi_driver
  end type mpi_driver

contains


  !> Initializes an mpi driver instance
  subroutine init_mpi_driver(this, mpienv)

    !> Instance
    type(mpi_driver), intent(out) :: this

    !> Mpi communicator to be used by the driver
    type(mpi_env), intent(in) :: mpienv

    type(mpi_context_factory) :: ctxfactory
    type(mpi_runner) :: runner

    call init_mpi_context_factory(ctxfactory, mpienv)
    call init_test_driver(this%test_driver, ctxfactory, runner)

  end subroutine init_mpi_driver


  !> Sets up an mpi test suite
  subroutine mpi_runner_set_up_suite(this, testsuite, ctx)

    !> Instance
    class(mpi_runner), intent(in) :: this

    !> Test suite to set up
    class(test_suite_base), pointer, intent(in) :: testsuite

    !> Context to use
    class(test_context), pointer, intent(in) :: ctx

    type(mpi_context), pointer :: pctx, poldctx
    class(mpi_suite_base), pointer :: psuite

    pctx => as_mpi_context(ctx)
    psuite => as_mpi_suite_base(testsuite)
    call set_mpi_global_context(pctx, oldctx=poldctx)
    call psuite%set_up()
    call set_mpi_global_context(poldctx)

  end subroutine mpi_runner_set_up_suite


  !> Sets up an mpi test suite
  subroutine mpi_runner_tear_down_suite(this, testsuite, ctx)

    !> Instance
    class(mpi_runner), intent(in) :: this

    !> Test suite to tear down
    class(test_suite_base), pointer, intent(in) :: testsuite

    !> Context to use
    class(test_context), pointer, intent(in) :: ctx

    type(mpi_context), pointer :: pctx, poldctx
    class(mpi_suite_base), pointer :: psuite

    pctx => as_mpi_context(ctx)
    psuite => as_mpi_suite_base(testsuite)
    call set_mpi_global_context(pctx, oldctx=poldctx)
    call psuite%tear_down()
    call set_mpi_global_context(poldctx)

  end subroutine mpi_runner_tear_down_suite


  !> Runs a test case
  subroutine mpi_runner_run_test(this, test, ctx)

    !> Instance
    class(mpi_runner), intent(in) :: this

    !> Test suite to tear down
    class(test_case_base), pointer, intent(in) :: test

    !> Context to use
    class(test_context), pointer, intent(in) :: ctx

    type(mpi_context), pointer :: pctx, poldctx
    class(mpi_case_base), pointer :: ptest

    pctx => as_mpi_context(ctx)
    ptest => as_mpi_case_base(test)
    call set_mpi_global_context(pctx, oldctx=poldctx)
    call ptest%run()
    call set_mpi_global_context(poldctx)

  end subroutine mpi_runner_run_test

end module fortuno_mpi_mpidriver