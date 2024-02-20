! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Types containing informations about tests and checks.
module fortuno_testinfo
  use fortuno_utils, only : as_char, nl
  implicit none

  private
  public :: check_result, failure_details, failure_info, test_result
  public :: init_failure_location, failure_location
  public :: init_drive_result, drive_result
  public :: teststatus, nteststatusvals


  !! Possible values for test status.
  type :: test_status_enum_
    integer :: notrun = 1
    integer :: succeeded = 2
    integer :: failed = 3
    integer :: skipped = 4
    integer :: ignored = 5
  end type test_status_enum_

  !> Possible test status values with following fields:
  !!
  !! notrun: test was not considered to be run yet
  !! succeeded: test was run and no error happened
  !! ignored: test had not been run due to previous errors (e.g. suite initializer failed)
  !! failed: test was run and some errors occured (e.g. check failed)
  !! skipped: test was deliberately skipped (e.g. skipped() called or initial conditions not met)
  !!
  type(test_status_enum_), parameter :: teststatus = test_status_enum_()

  !> Nr. of possible values for a test status (nr. of field in the teststatus instance)
  integer, parameter :: nteststatusvals = 5


  !> Contains details about a failed check.
  type, abstract :: failure_details
  contains
    procedure(failure_details_as_char), deferred :: as_char
  end type failure_details


  abstract interface

    !> Character representation of the failure details (typically the failure reason).
    function failure_details_as_char(this) result(repr)
      import :: failure_details
      implicit none

      !> instance
      class(failure_details), intent(in) :: this

      !> character representation
      character(:), allocatable :: repr

    end function failure_details_as_char

  end interface


  !> Contains the result of a check.
  type :: check_result

    !> whether the check was successful
    logical :: success = .false.

    !> further information about the check (reason of failure)
    class(failure_details), allocatable :: details

  end type check_result


  !> Location of the failure, to be extended by specific drivers depending on available information
  type :: failure_location

    !> source file containin the check
    character(:), allocatable :: file

    !> line number of the check
    integer :: line = 0

    !> check number (order number within context), zero if failure was not due to a failing check
    integer :: checknr = 0

  contains
    procedure :: as_char => failure_location_as_char
  end type failure_location


  !> Contains all collected information about failed check(s).
  type :: failure_info

    !> message associated with the check, usually provided by the user
    character(:), allocatable :: message

    !> failure location information
    class(failure_location), allocatable :: location

    !> internal details of the check (with method to render it as text)
    class(failure_details), allocatable :: details

    !> contains previous failure_info (to be able to chain check infos)
    type(failure_info), allocatable :: previous

  end type failure_info


  !> Represents the result of a test case or an initializer/finalizer of a test suite.
  type :: test_result

    !> name of the test
    character(:), allocatable :: name

    !> name of the test containing also the internal representation of the components, if present
    character(:), allocatable :: reprname

    !> final status of the test
    integer :: status = teststatus%notrun

    !> information about the failure (in case the test failed)
    type(failure_info), allocatable :: failureinfo

    !> indices of the dependencies, the immediate dependency (containing suite) first
    integer, allocatable :: dependencies(:)

  end type test_result


  !> Contains all results obtained after tests had been driven via a driver.
  type :: drive_result

    !> results of the individual test runs.
    type(test_result), allocatable :: testresults(:)

    !> results of the suite set-ups (1,:) and tear-downs (2, :).
    type(test_result), allocatable :: suiteresults(:,:)

    !> whether the test run was successful (all tests either successful or skipped)
    logical :: successful = .false.

    !> nr. of suite results with given status, shape (nteststatusvalue, 2)
    integer, allocatable :: suitestats(:,:)

    !> nr. of test results with given status, shape (nteststatusvalue,)
    integer, allocatable :: teststats(:)

  contains

    procedure :: calculate_stats => drive_result_calculate_stats

  end type drive_result

contains

  !> Initializes a failure location instance
  subroutine init_failure_location(this, checknr, file, line)

    !> instance
    type(failure_location), intent(out) :: this

    !> nr. of checks made so far
    integer, intent(in) :: checknr

    !> file where failure occured (if available)
    character(*), optional, intent(in) :: file

    !> line where failure occured (if available)
    integer, optional, intent(in) :: line

    this%checknr = checknr
    if (present(file)) this%file = file
    if (present(line)) this%line = line

  end subroutine init_failure_location


  !> Character representation of the failure location.
  function failure_location_as_char(this) result(repr)

    !> instance
    class(failure_location), intent(in) :: this

    !> character representation
    character(:), allocatable :: repr

    if (allocated(this%file)) then
      repr = "File: " // this%file
      if (this%line /= 0) then
        repr = repr // " (line " // as_char(this%line) // ")"
      else if (this%checknr /= 0) then
        repr = repr // " (check " // as_char(this%checknr) // ")"
      end if
    else if (this%checknr /= 0) then
      repr = "Check: " // as_char(this%checknr)
    else
      repr = ""
    end if

  end function failure_location_as_char


  !> Initializes the fields of an drive result instance
  subroutine init_drive_result(this, nsuites, ntests)
    type(drive_result), intent(out) :: this
    integer, intent(in) :: nsuites, ntests

    allocate(this%suiteresults(2, nsuites))
    allocate(this%testresults(ntests))
    allocate(this%suitestats(nteststatusvals, 2), source=0)
    allocate(this%teststats(nteststatusvals), source=0)

  end subroutine init_drive_result


  !> Calculates the statistics using the already stored results
  subroutine drive_result_calculate_stats(this)

    !> Instance
    class(drive_result), intent(inout) :: this

    integer :: ii
    logical :: testsok, suitesok(2)

    do ii = 1, nteststatusvals
      this%suitestats(ii, 1) = count(this%suiteresults(1, :)%status == ii)
      this%suitestats(ii, 2) = count(this%suiteresults(2, :)%status == ii)
      this%teststats(ii) = count(this%testresults(:)%status == ii)
    end do
    do ii = 1, 2
      associate(stats => this%suitestats(:, ii))
        suitesok(ii) = stats(teststatus%succeeded) + stats(teststatus%skipped) == sum(stats)
      end associate
    end do
    associate(stats => this%teststats)
      testsok = stats(teststatus%succeeded) + stats(teststatus%skipped) == sum(stats)
    end associate
    this%successful = suitesok(1) .and. suitesok(2) .and. testsok

  end subroutine drive_result_calculate_stats

end module fortuno_testinfo
