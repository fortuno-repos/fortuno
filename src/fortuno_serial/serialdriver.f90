! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Implements a driver for serial tests
module fortuno_serial_serialdriver
  use fortuno, only : init_test_driver, test_case_base, test_context, test_driver,&
      & test_runner, test_suite_base
  use fortuno_serial_serialbasetypes, only : as_serial_case_base, as_serial_suite_base,&
      & serial_case_base, serial_suite_base
  use fortuno_serial_serialcontext, only : as_serial_context, serial_context, serial_context_factory
  use fortuno_serial_serialglobalctx, only : set_serial_global_context
  implicit none

  private
  public :: init_serial_driver, serial_driver


  !> Runner implementation for serial tests and test suites
  type, extends(test_runner) :: serial_runner
  contains
    procedure :: set_up_suite => serial_runner_set_up_suite
    procedure :: tear_down_suite => serial_runner_tear_down_suite
    procedure :: run_test => serial_runner_run_test
  end type serial_runner


  !> Driver for serial tests
  type, extends(test_driver) :: serial_driver
  end type serial_driver

contains

  !> Initializes a serial driver instance
  subroutine init_serial_driver(this)

    !> Instance
    type(serial_driver), intent(out) :: this

    type(serial_context_factory) :: ctxfactory
    type(serial_runner) :: runner

    call init_test_driver(this%test_driver, ctxfactory, runner)

  end subroutine init_serial_driver


  !> Sets up a serial test suite
  subroutine serial_runner_set_up_suite(this, testsuite, ctx)

    !> Instance
    class(serial_runner), intent(in) :: this

    !> Test suite to set up
    class(test_suite_base), pointer, intent(in) :: testsuite

    !> Context to use
    class(test_context), pointer, intent(in) :: ctx

    type(serial_context), pointer :: pctx, poldctx
    class(serial_suite_base), pointer :: psuite

    pctx => as_serial_context(ctx)
    psuite => as_serial_suite_base(testsuite)
    call set_serial_global_context(pctx, oldctx=poldctx)
    call psuite%set_up()
    call set_serial_global_context(poldctx)

  end subroutine serial_runner_set_up_suite


  !> Sets up a serial test suite
  subroutine serial_runner_tear_down_suite(this, testsuite, ctx)

    !> Instance
    class(serial_runner), intent(in) :: this

    !> Test suite to tear down
    class(test_suite_base), pointer, intent(in) :: testsuite

    !> Context to use
    class(test_context), pointer, intent(in) :: ctx

    type(serial_context), pointer :: pctx, poldctx
    class(serial_suite_base), pointer :: psuite

    pctx => as_serial_context(ctx)
    psuite => as_serial_suite_base(testsuite)
    call set_serial_global_context(pctx, oldctx=poldctx)
    call psuite%tear_down()
    call set_serial_global_context(poldctx)

  end subroutine serial_runner_tear_down_suite


  !> Runs a test case
  subroutine serial_runner_run_test(this, test, ctx)

    !> Instance
    class(serial_runner), intent(in) :: this

    !> Test suite to tear down
    class(test_case_base), pointer, intent(in) :: test

    !> Context to use
    class(test_context), pointer, intent(in) :: ctx

    type(serial_context), pointer :: pctx, poldctx
    class(serial_case_base), pointer :: ptest

    pctx => as_serial_context(ctx)
    ptest => as_serial_case_base(test)
    call set_serial_global_context(pctx, oldctx=poldctx)
    call ptest%run()
    call set_serial_global_context(poldctx)

  end subroutine serial_runner_run_test

end module fortuno_serial_serialdriver