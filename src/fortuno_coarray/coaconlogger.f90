! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains the implementation of the test logger for the coarray driver
module fortuno_coarray_coaconlogger
  use fortuno, only : as_char, console_logger, failure_info, nl
  use fortuno_coarray_coaenv, only : coa_env
  use fortuno_coarray_coatestinfo, only : coa_failure_location
  implicit none

  private
  public :: init_coa_console_logger, coa_console_logger


  !> Implements a coarray logger which logs to a formatted unit
  type, extends(console_logger) :: coa_console_logger
    type(coa_env) :: coaenv
  contains
    procedure :: is_active => coa_console_logger_is_active
    procedure :: start_drive => coa_console_logger_start_drive
    procedure :: get_failure_info_repr => coa_console_logger_get_failure_info_repr
  end type coa_console_logger

contains

  !> Initializes an coa_console_logger instance
  subroutine init_coa_console_logger(this, coaenv)

    !> Instance
    type(coa_console_logger), intent(out) :: this

    !> coa environment
    type(coa_env), intent(in) :: coaenv

    this%coaenv = coaenv

  end subroutine init_coa_console_logger


  !> Returns whether the logger is active
  function coa_console_logger_is_active(this) result(isactive)

    !> Instance
    class(coa_console_logger), intent(in) :: this

    !> Whether logger is active
    logical :: isactive

    isactive = this%coaenv%image == 1

  end function coa_console_logger_is_active


  !> Starts the logging, called before any other procedures are called
  subroutine coa_console_logger_start_drive(this)

    !> Instance
    class(coa_console_logger), intent(inout) :: this

    if (.not. this%is_active()) return
    call this%console_logger%start_drive()
    call this%log_message(nl // "Nr. of images: " // as_char(this%coaenv%nimages))

  end subroutine coa_console_logger_start_drive


  !> Returns the representation of the failure information (called collectively)
  subroutine coa_console_logger_get_failure_info_repr(this, failureinfo, location, message,&
      & details)

    !> Instance
    class(coa_console_logger), intent(in) :: this

    !> Failure info to get the representation from
    type(failure_info), allocatable, intent(in) :: failureinfo

    !> Location string (unallocated if not available or not relevant)
    character(:), allocatable, intent(out) :: location

    !> Message string (unallocated if not available or not relevant)
    character(:), allocatable, intent(out) :: message

    !> Details string (unallocated if not available or not relevant)
    character(:), allocatable, intent(out) :: details

    character(:), allocatable :: buffer
    integer, allocatable :: failedimages(:)

    select type (loc => failureinfo%location)
    class is (coa_failure_location)
      failedimages = loc%failedimages
    class default
      error stop "invalid failureinfo%location type in coa_console_logger_get_failure_info_repr"
    end select

    if (this%coaenv%image == failedimages(1)) then
      location = failureinfo%location%as_char()
      if (len(location) == 0) deallocate(location)
      if (allocated(failureinfo%message)) message = failureinfo%message
      if (allocated(failureinfo%details)) details = failureinfo%details%as_char()
    end if
    call this%coaenv%broadcast_alloc_char(location, failedimages(1))
    call this%coaenv%broadcast_alloc_char(message, failedimages(1))
    call this%coaenv%broadcast_alloc_char(details, failedimages(1))

  end subroutine coa_console_logger_get_failure_info_repr

end module fortuno_coarray_coaconlogger
