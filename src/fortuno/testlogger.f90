! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains a generic logger to be overriden by specific implementations
module fortuno_testlogger
  use fortuno_testinfo, only : drive_result, test_result
  implicit none

  private
  public :: test_logger, testtypes


  !! Helper type for testtypes.
  type :: test_types_enum_
    integer :: suitesetup = 1
    integer :: suiteteardown = 2
    integer :: testrun = 3
  end type test_types_enum_

  !> Contains possible choices for the origin of a test result object
  !!
  !! Possible fields are:
  !! * suitesetup -- result of a set_up() routine of a test suite
  !! * suiteteardown -- result of a tear_down() routine of a test suite
  !! * testrun -- result of a test_case run() routine.
  !!
  type(test_types_enum_), parameter :: testtypes = test_types_enum_()


  !> Contains the definition of a generic test logger
  type, abstract :: test_logger
  contains
    procedure(test_logger_log_message), deferred :: log_message
    procedure(test_logger_log_error), deferred :: log_error
    procedure(test_logger_start_drive), deferred :: start_drive
    procedure(test_logger_end_drive), deferred :: end_drive
    procedure(test_logger_start_tests), deferred :: start_tests
    procedure(test_logger_end_tests), deferred :: end_tests
    procedure(test_logger_log_test_result), deferred :: log_test_result
    procedure(test_logger_log_drive_result), deferred :: log_drive_result
  end type test_logger


  abstract interface

    !> Logs a normal message
    subroutine test_logger_log_message(this, message)
      import test_logger
      implicit none

      !> Instance
      class(test_logger), intent(inout) :: this

      !> Message to log
      character(*), intent(in) :: message

    end subroutine test_logger_log_message


    !> Logs an error message
    subroutine test_logger_log_error(this, message)
      import test_logger
      implicit none

      !> Instance
      class(test_logger), intent(inout) :: this

      !> Message to log
      character(*), intent(in) :: message

    end subroutine test_logger_log_error


    !> Called when the test drive starts (e.g. for printing headers)
    subroutine test_logger_start_drive(this)
      import test_logger
      implicit none

      !> Instance
      class(test_logger), intent(inout) :: this

    end subroutine test_logger_start_drive


    !> Called then the test drive ended
    subroutine test_logger_end_drive(this)
      import test_logger
      implicit none

      !> Instance
      class(test_logger), intent(inout) :: this

    end subroutine test_logger_end_drive


    !> Called immediately before the processing of the tests starts
    subroutine test_logger_start_tests(this)
      import test_logger
      implicit none

      !> Instance
      class(test_logger), intent(inout) :: this

    end subroutine test_logger_start_tests


    !> Called after the processing of all tests had been finished
    subroutine test_logger_end_tests(this)
      import test_logger
      implicit none

      !> Instance
      class(test_logger), intent(inout) :: this

    end subroutine test_logger_end_tests


    !> Logs the result of an individual test during processing
    subroutine test_logger_log_test_result(this, testtype, testresult)
      import test_logger, test_result
      implicit none

      !> Instance
      class(test_logger), intent(inout) :: this

      !> Type of the test to be logged (suitesetup, suiteteardown, testrun)
      integer, intent(in) :: testtype

      !> Result of the test to log
      type(test_result), intent(in) :: testresult

    end subroutine test_logger_log_test_result


    !> Logs the final detailed summary after all tests test drive has finished
    subroutine test_logger_log_drive_result(this, driveresult)
      import test_logger, drive_result
      implicit none

      !> Instance
      class(test_logger), intent(inout) :: this

      !> Results of suite initializers and finalizers (shape: (2, ntestsuites))
      type(drive_result), intent(in) :: driveresult

    end subroutine test_logger_log_drive_result

  end interface

end module fortuno_testlogger