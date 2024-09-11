! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Implements a driver for coarray tests
module fortuno_coarray_coadriver
  use fortuno, only : init_test_driver, test_case_base, test_context, test_driver, test_runner,&
      & test_suite_base
  use fortuno_coarray_coabasetypes, only : as_coa_pure_case_base, as_coa_pure_suite_base,&
      & coa_pure_case_base, coa_pure_suite_base
  use fortuno_coarray_coacontext, only : as_coa_context, init_coa_context_factory, coa_context,&
      & coa_context_factory
  use fortuno_coarray_coaenv, only : coa_env
  implicit none

  private
  public :: init_coa_driver, coa_driver


  !> Runner implementation for coarray tests and test suites
  type, extends(test_runner) :: coa_runner
  contains
    procedure :: set_up_suite => coa_runner_set_up_suite
    procedure :: tear_down_suite => coa_runner_tear_down_suite
    procedure :: run_test => coa_runner_run_test
  end type coa_runner


  !> Driver for coarray tests
  type, extends(test_driver) :: coa_driver
  end type coa_driver

contains


  !> Initializes a coarray driver instance
  subroutine init_coa_driver(this, coaenv)

    !> Instance
    type(coa_driver), intent(out) :: this

    !> Coarray communicator to be used by the driver
    type(coa_env), intent(in) :: coaenv

    type(coa_context_factory) :: ctxfactory
    type(coa_runner) :: runner

    call init_coa_context_factory(ctxfactory, coaenv)
    call init_test_driver(this%test_driver, ctxfactory, runner)

  end subroutine init_coa_driver


  !> Sets up a coarray test suite
  subroutine coa_runner_set_up_suite(this, testsuite, ctx)

    !> Instance
    class(coa_runner), intent(in) :: this

    !> Test suite to set up
    class(test_suite_base), pointer, intent(in) :: testsuite

    !> Context to use
    class(test_context), pointer, intent(in) :: ctx

    type(coa_context), pointer :: pctx, poldctx
    class(coa_pure_suite_base), pointer :: psuite

    pctx => as_coa_context(ctx)
    psuite => as_coa_pure_suite_base(testsuite)
    call psuite%set_up(pctx)

  end subroutine coa_runner_set_up_suite


  !> Sets up a coarray test suite
  subroutine coa_runner_tear_down_suite(this, testsuite, ctx)

    !> Instance
    class(coa_runner), intent(in) :: this

    !> Test suite to tear down
    class(test_suite_base), pointer, intent(in) :: testsuite

    !> Context to use
    class(test_context), pointer, intent(in) :: ctx

    type(coa_context), pointer :: pctx, poldctx
    class(coa_pure_suite_base), pointer :: psuite

    pctx => as_coa_context(ctx)
    psuite => as_coa_pure_suite_base(testsuite)
    call psuite%tear_down(pctx)

  end subroutine coa_runner_tear_down_suite


  !> Runs a test case
  subroutine coa_runner_run_test(this, test, ctx)

    !> Instance
    class(coa_runner), intent(in) :: this

    !> Test suite to tear down
    class(test_case_base), pointer, intent(in) :: test

    !> Context to use
    class(test_context), pointer, intent(in) :: ctx

    type(coa_context), pointer :: pctx, poldctx
    class(coa_pure_case_base), pointer :: ptest

    pctx => as_coa_context(ctx)
    ptest => as_coa_pure_case_base(test)
    call ptest%run(pctx)

  end subroutine coa_runner_run_test

end module fortuno_coarray_coadriver