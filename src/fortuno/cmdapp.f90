! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains common code used by the various command line apps
module fortuno_cmdapp
  use fortuno_argumentparser, only : argtypes, argument_def, argument_values, argument_parser,&
      & init_argument_parser
  use fortuno_basetypes, only : test_list
  use fortuno_utils, only : string_item
  use fortuno_testdriver, only : test_driver, test_selection
  use fortuno_testlogger, only : test_logger
  implicit none

  private
  public :: cmd_app
  public :: get_selections
  public :: default_argument_defs


  !> App for driving tests through command line interface app
  type :: cmd_app
    class(test_logger), allocatable :: logger
    class(test_driver), allocatable :: driver
    type(argument_values) :: argvals
  contains
    procedure :: run => cmd_app_run
    procedure :: parse_args => cmd_app_parse_args
    procedure :: register_tests => cmd_app_register_tests
    procedure :: run_tests => cmd_app_run_tests
  end type cmd_app


contains

  !> Runs the command line interface app (calls parse_args(), register_tests() and run_tests())
  subroutine cmd_app_run(this, tests, exitcode)

    !> Instance
    class(cmd_app), intent(inout) :: this

    !> Test items to be considered by the app
    type(test_list), intent(in) :: tests

    !> Exit code of the run
    integer, intent(out) :: exitcode

    call this%parse_args(exitcode)
    if (exitcode >= 0) return
    call this%register_tests(tests, exitcode)
    if (exitcode >= 0) return
    call this%run_tests(exitcode)

  end subroutine cmd_app_run


  !> Parses the command line arguments
  subroutine cmd_app_parse_args(this, exitcode)

    !> Instance
    class(cmd_app), intent(inout) :: this

    !> Exit code (-1 if processing can continue, >=0 if program should stop with that exit code)
    integer, intent(out) :: exitcode

    type(argument_parser) :: argparser

    call init_argument_parser(argparser,&
        & description="Command line app for driving Fortuno unit tests.",&
        & argdefs=default_argument_defs()&
        & )
    call argparser%parse_args(this%argvals, this%logger, exitcode)

  end subroutine cmd_app_parse_args


  !> Register all tests which should be considered
  subroutine cmd_app_register_tests(this, testitems, exitcode)

    !> Initialized instance on exit
    class(cmd_app), intent(inout) :: this

    !> Items to be considered by the app
    type(test_list), intent(in) :: testitems

    !> Exit code (-1, if processing can continue, >= 0 otherwise)
    integer, intent(out) :: exitcode

    type(test_selection), allocatable :: selections(:)
    type(string_item), allocatable :: selectors(:), testnames(:)
    integer :: itest

    exitcode = -1
    if (this%argvals%has("tests")) then
      call this%argvals%get_value("tests", selectors)
      call get_selections(selectors, selections)
    end if
    call this%driver%register_tests(testitems, selections=selections)

    if (this%argvals%has("list")) then
      call this%driver%get_test_names(testnames)
      do itest = 1, size(testnames)
        call this%logger%log_message(testnames(itest)%value)
      end do
      exitcode = 0
      return
    end if

  end subroutine cmd_app_register_tests


  !> Runs the initialized app
  subroutine cmd_app_run_tests(this, exitcode)

    !> Instance
    class(cmd_app), intent(inout) :: this

    !> Exit code of the test run (0 - success, 1 - failure)
    integer, intent(out) :: exitcode

    call this%driver%run_tests(this%logger)
    if (this%driver%driveresult%successful) then
      exitcode = 0
    else
      exitcode = 1
    end if

  end subroutine cmd_app_run_tests


  !> Converts test selector expressions to test selections
  subroutine get_selections(selectors, selections)

    !> Selector expressions
    type(string_item), intent(in) :: selectors(:)

    !> Array of selections on exit
    type(test_selection), allocatable, intent(out) :: selections(:)

    integer :: ii

    allocate(selections(size(selectors)))
    do ii = 1, size(selectors)
      associate(selector => selectors(ii)%value, selection => selections(ii))
        if (selector(1:1) == "~") then
          selection%name = selector(2:)
          selection%selectiontype = "-"
        else
          selection%name = selector
          selection%selectiontype = "+"
        end if
      end associate
    end do

  end subroutine get_selections


  !> Returns the default argument definitions for the command line apps
  function default_argument_defs() result(argdefs)

    !> Argument defintions
    type(argument_def), allocatable :: argdefs(:)

    ! Workaround:gfortran:14.1 (bug 116679)
    ! Omit array expression to avoid memory leak
    ! {-
    ! argdefs = [&
    !     & &
    !     & argument_def("list", argtypes%bool, shortopt="l", longopt="list",&
    !     & helpmsg="show list of tests to run and exit"),&
    !     & &
    !     & argument_def("tests", argtypes%stringlist,&
    !     & helpmsg="list of tests and suites to include or to exclude when prefixed with '~' (e.g.&
    !     & 'somesuite ~somesuite/avoidedtest' would run all tests except 'avoidedtest' in the test&
    !     & suite 'somesuite')")&
    !     & &
    !     & ]
    ! -}{+
    allocate(argdefs(2))
    argdefs(1) =  argument_def("list", argtypes%bool, shortopt="l", longopt="list",&
        & helpmsg="show list of tests to run and exit")
    argdefs(2) = argument_def("tests", argtypes%stringlist,&
        & helpmsg="list of tests and suites to include or to exclude when prefixed with '~' (e.g.&
        & 'somesuite ~somesuite/avoidedtest' would run all tests except 'avoidedtest' in the test&
        & suite 'somesuite')")
    ! +}

  end function default_argument_defs

end module fortuno_cmdapp
