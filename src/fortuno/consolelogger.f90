! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains the implementation of the test logger for logging on the console
module fortuno_consolelogger
  use fortuno_env, only : ansicolors, stderr, stdout
  use fortuno_testinfo, only : drive_result, failure_info, test_result, teststatus
  use fortuno_testlogger, only : test_logger
  use fortuno_utils, only : str
  implicit none

  private
  public :: console_logger


  !> Implements a logger which logs to the console
  type, extends(test_logger) :: console_logger
  contains
    procedure :: is_active => console_logger_is_active
    procedure :: get_failure_info_repr => console_logger_get_failure_info_repr
    procedure :: log_message => console_logger_log_message
    procedure :: log_error => console_logger_log_error
    procedure :: start_drive => console_logger_start_drive
    procedure :: end_drive => console_logger_end_drive
    procedure :: start_tests => console_logger_start_tests
    procedure :: end_tests => console_logger_end_tests
    procedure :: log_test_result => console_logger_log_test_result
    procedure :: log_drive_result => console_logger_log_drive_result
  end type console_logger


  character(*), parameter :: notrun_short_ = "!"
  character(*), parameter :: succeeded_short_ = "."
  character(*), parameter :: failed_short_ = "F"
  character(*), parameter :: skipped_short_ = "s"
  character(*), parameter :: ignored_short_ = "I"
  character(*), parameter :: unknown_short_ = "?"

  character(*), parameter :: notrun_long_ =&
      & ansicolors%red // "Not run  " // ansicolors%default

  character(*), parameter :: succeeded_long_ =&
      & ansicolors%default // "OK       " // ansicolors%default

  character(*), parameter :: failed_long_ = &
      & ansicolors%red // "Failed   " // ansicolors%default

  character(*), parameter :: skipped_long_ =&
      & ansicolors%cyan // "Skipped  " // ansicolors%default

  character(*), parameter :: ignored_long_ =&
      & ansicolors%magenta // "Ignored  " // ansicolors%default

  character(*), parameter :: unknown_long_ =&
      & ansicolors%yellow // "???????  " // ansicolors%default

contains


  !> Returns whether the logger is active
  function console_logger_is_active(this) result(isactive)

    !> Instance
    class(console_logger), intent(in) :: this

    !> Whether logger is active
    logical :: isactive

    isactive = .true.

  end function console_logger_is_active


  !> Returns the representation of the failure information (called collectively)
  subroutine console_logger_get_failure_info_repr(this, failureinfo, location, message,&
      & details)

    !> Instance
    class(console_logger), intent(in) :: this

    !> Failure info to get the representation from
    type(failure_info), allocatable, intent(in) :: failureinfo

    !> Location string (unallocated if not available or not relevant)
    character(:), allocatable, intent(out) :: location

    !> Message string (unallocated if not available or not relevant)
    character(:), allocatable, intent(out) :: message

    !> Details string (unallocated if not available or not relevant)
    character(:), allocatable, intent(out) :: details

    location = failureinfo%location%as_string()
    if (allocated(failureinfo%message)) message = failureinfo%message
    if (allocated(failureinfo%details)) details = failureinfo%details%as_string()

  end subroutine console_logger_get_failure_info_repr


  !> Logs a normal message
  subroutine console_logger_log_message(this, message)

    !> Instance
    class(console_logger), intent(inout) :: this

    !> Message to log
    character(*), intent(in) :: message

    if (.not. this%is_active()) return
    write(stdout, "(a)") message

  end subroutine console_logger_log_message


  !> Logs an error message
  subroutine console_logger_log_error(this, message)

    !> Instance
    class(console_logger), intent(inout) :: this

    !> Message to log
    character(*), intent(in) :: message

    if (.not. this%is_active()) return
    write(stderr, "(a)") message

  end subroutine console_logger_log_error


  !> Starts the logging, called before any other procedures are called
  subroutine console_logger_start_drive(this)

    !> Instance
    class(console_logger), intent(inout) :: this

    if (.not. this%is_active()) return
    write(stdout, "(a)") "=== Fortuno - flextensible unit testing framework for Fortran ==="

  end subroutine console_logger_start_drive


  !> Ends the logging, called when no more logging is needed
  subroutine console_logger_end_drive(this)

    !> Instance
    class(console_logger), intent(inout) :: this

  end subroutine console_logger_end_drive


  !> Starts the processing log, called immediately before the processing of the tests starts
  subroutine console_logger_start_tests(this)

    !> Instance
    class(console_logger), intent(inout) :: this

    if (.not. this%is_active()) return
    write(stdout, "(/, a)") "# Executing test items"

  end subroutine console_logger_start_tests


  !> Ends processing log, called after the processing of all tests had been finished
  subroutine console_logger_end_tests(this)

    !> Instance
    class(console_logger), intent(inout) :: this

    if (.not. this%is_active()) return
    write(stdout, "()")

  end subroutine console_logger_end_tests


  !> Logs the result of an individual test during processing
  subroutine console_logger_log_test_result(this, testtype, testresult)

    !> Instance
    class(console_logger), intent(inout) :: this

    !> Type of the test to be logged (suitesetup, suiteteardown, testrun)
    integer, intent(in) :: testtype

    !> Result of the test to log
    type(test_result), intent(in) :: testresult

    if (.not. this%is_active()) return
    write(stdout, "(a)", advance="no") status_short_repr_(testresult%status)

  end subroutine console_logger_log_test_result


  !> Logs the final detailed summary after all tests had been run
  subroutine console_logger_log_drive_result(this, driveresult)

    !> Instance
    class(console_logger), intent(inout) :: this

    !> Results of suite initializers and finalizers (shape: (2, ntestsuites))
    type(drive_result), intent(in) :: driveresult

    integer :: numfieldwidth, maxitems

    ! event logging might need collective communication of all loggers
    call log_events_(this, driveresult%suiteresults, driveresult%testresults)
    if (.not. this%is_active()) return

    maxitems = maxval([sum(driveresult%suitestats, dim=1), sum(driveresult%teststats)])
    numfieldwidth = len(str(maxitems))
    call log_summary_("# Suite set-ups", driveresult%suiteresults(1, :),&
        & driveresult%suitestats(:, 1), numfieldwidth)
    call log_summary_("# Suite tear-downs", driveresult%suiteresults(2, :),&
        & driveresult%suitestats(:, 2), numfieldwidth)
    call log_summary_("# Test runs", driveresult%testresults, driveresult%teststats,&
        & numfieldwidth)
    call log_success_(driveresult%successful)

  end subroutine console_logger_log_drive_result


  !! Returns a single character representation of the test status.
  pure function status_short_repr_(status) result(repr)
    integer, intent(in) :: status
    character(:), allocatable :: repr

    select case (status)
    case (teststatus%notrun)
      repr = notrun_short_
    case (teststatus%succeeded)
      repr = succeeded_short_
    case (teststatus%failed)
      repr = failed_short_
    case (teststatus%skipped)
      repr = skipped_short_
    case (teststatus%ignored)
      repr = ignored_short_
    case default
      repr = unknown_short_
    end select

  end function status_short_repr_


  !! Returns a single character representation of the test status.
  pure function status_long_repr_(status) result(repr)
    integer, intent(in) :: status
    character(:), allocatable :: repr

    select case (status)
    case (teststatus%notrun)
      repr = notrun_long_
    case (teststatus%succeeded)
      repr = succeeded_long_
    case (teststatus%failed)
      repr = failed_long_
    case (teststatus%skipped)
      repr = skipped_long_
    case (teststatus%ignored)
      repr = ignored_long_
    case default
      repr = unknown_long_
    end select

  end function status_long_repr_


  !! Logs failure events.
  subroutine log_events_(this, suiteresults, testresults)

    !> Instance
    class(console_logger), intent(inout) :: this

    !> Results of suite initializers and finalizers (shape: (2, ntestsuites))
    type(test_result), intent(in) :: suiteresults(:,:)

    !> Results of test runs (shape: (ntestcases))
    type(test_result), intent(in) :: testresults(:)

    integer :: ii

    if (any(suiteresults(1, :)%status /= teststatus%succeeded)&
        & .or. any(testresults(:)%status /= teststatus%succeeded)&
        & .or. any(suiteresults(2, :)%status /= teststatus%succeeded)) then
      if (this%is_active()) write(stdout, "(/, a)") "# Logged event(s)"
    end if

    if (any(suiteresults(1, :)%status /= teststatus%succeeded)) then
      do ii = 1, size(suiteresults, dim=2)
        associate (res => suiteresults(1, ii))
          if (res%status == teststatus%succeeded) cycle
          if (this%is_active()) then
            if (res%status == teststatus%failed) write(stdout, "()")
            write(stdout, "(a, 2x, 2a)") status_long_repr_(res%status), "[set-up] ", res%reprname
          end if
          if (res%status == teststatus%failed) then
            ! write_failure_info() needs collective communication, must be called by all loggers
            call write_failure_info_(this, res%failureinfo)
            if (this%is_active()) write(stdout, "()")
          end if
        end associate
      end do
    end if

    if (any(testresults(:)%status /= teststatus%succeeded)) then
      do ii = 1, size(testresults)
        associate (res => testresults(ii))
          if (res%status == teststatus%succeeded) cycle
          if (this%is_active()) then
            if (res%status == teststatus%failed) write(stdout, "()")
            write(stdout, "(a, 2x, 2a)") status_long_repr_(res%status), "[run] ", res%reprname
          end if
          if (res%status == teststatus%failed) then
            ! write_failure_info() needs collective communication, must be called by all loggers
            call write_failure_info_(this, res%failureinfo)
            if (this%is_active()) write(stdout, "()")
          end if
        end associate
      end do
    end if

    if (any(suiteresults(2, :)%status /= teststatus%succeeded)) then
      do ii = 1, size(suiteresults, dim=2)
        associate (res => suiteresults(2, ii))
          if (res%status == teststatus%succeeded) cycle
          if (this%is_active()) then
            if (res%status == teststatus%failed) write(stdout, "()")
            write(stdout, "(a, 2x, 2a)") status_long_repr_(res%status), "[tear-down] ", res%reprname
          end if
          if (res%status == teststatus%failed) then
            ! write_failure_info() needs collective communication, must be called by all loggers
            call write_failure_info_(this, res%failureinfo)
            if (this%is_active()) write(stdout, "()")
          end if
        end associate
      end do
    end if

  end subroutine log_events_


  !! Writes a failure info.
  recursive subroutine write_failure_info_(this, failureinfo)
    class(console_logger), intent(in) :: this
    type(failure_info), allocatable, intent(in) :: failureinfo

    character(:), allocatable :: location, message, details

    if (.not. allocated(failureinfo)) return
    call write_failure_info_(this, failureinfo%previous)
    if (failureinfo%location%checknr /= 0) then
      if (this%is_active()) &
          & write(stdout, "(/, a)") "-> Unsuccessful check"
    else
      if (this%is_active()) &
          & write(stdout, "(/, a)") "-> Failure"
    end if
    call this%get_failure_info_repr(failureinfo, location, message, details)
    if (this%is_active()) then
      if (allocated(location)) write(stdout, "(a)") location
      if (allocated(message)) write(stdout, "(2a)") "Msg: ", message
      if (allocated(details)) write(stdout, "(a)") details
    end if

  end subroutine write_failure_info_


  !! Logs test summary
  subroutine log_summary_(header, testresults, teststats, numfieldwidth)
    character(*), intent(in) :: header
    type(test_result), intent(in) :: testresults(:)
    integer, intent(in) :: teststats(:)
    integer, intent(in) :: numfieldwidth

    integer :: ntotal, ntotal2, nnotrun, nsucceeded, nfailed, nskipped, nignored
    character(:), allocatable :: formatstr1, formatstr2, numfieldwidthstr

    ntotal = sum(teststats)
    if (ntotal == 0) return
    nnotrun = teststats(teststatus%notrun)
    nsucceeded = teststats(teststatus%succeeded)
    nfailed = teststats(teststatus%failed)
    nskipped = teststats(teststatus%skipped)
    nignored = teststats(teststatus%ignored)
    ntotal2 = ntotal - nskipped

    numfieldwidthstr = str(numfieldwidth)
    formatstr1 = "(a, 2x, i" //  numfieldwidthstr // ")"
    formatstr2 = "(a, 2x, i" //  numfieldwidthstr // ", 1x, a, f5.1, a)"

    write(stdout, "(/, a)") header
    write(stdout, formatstr1) "Total:    ", ntotal
    if (nskipped > 0) then
      write(stdout, formatstr2) "Skipped:  ", nskipped, " (", 100.0 * real(nskipped) / ntotal, "%)"
    end if
    write(stdout, formatstr2) "Succeeded:", nsucceeded, " (", 100.0 * real(nsucceeded) / ntotal,&
        & "%)"
    if (nfailed > 0) then
      write(stdout, formatstr2) "Failed:   ", nfailed, " (", 100.0 * real(nfailed) / ntotal, "%)"
    end if
    if (nignored > 0) then
      write(stdout, formatstr2) "Ignored:  ", nignored, " (", 100.0 * real(nignored) / ntotal, "%)"
    end if

  end subroutine log_summary_


  !! Log the success status of the test process
  subroutine log_success_(successful)
    logical, intent(in) :: successful

    write(stdout, "(/, a)", advance="no") "=== "
    if (successful) then
      write(stdout, "(4a)", advance="no") ansicolors%green, "Succeeded", ansicolors%default
    else
      write(stdout, "(4a)", advance="no") ansicolors%red, "FAILED", ansicolors%default
    end if
    write(stdout, "(a)") " ==="

  end subroutine log_success_

end module fortuno_consolelogger