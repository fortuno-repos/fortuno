! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains a serial logger implementation
module fortuno_serial_serialconlogger
  use fortuno_consolelogger, only : console_logger
  use fortuno_testinfo, only : failure_info
  implicit none

  private
  public :: serial_console_logger


  !> Serial logger
  type, extends(console_logger) :: serial_console_logger
  contains
    procedure :: is_active => serial_console_logger_is_active
    procedure :: get_failure_info_repr => serial_console_logger_get_failure_info_repr
  end type serial_console_logger

contains

  !> Returns whether the logger is active
  function serial_console_logger_is_active(this) result(isactive)

    !> Instance
    class(serial_console_logger), intent(in) :: this

    !> Whether logger is active
    logical :: isactive

    isactive = .true.

  end function serial_console_logger_is_active


  !> Returns the representation of the failure information (called collectively)
  subroutine serial_console_logger_get_failure_info_repr(this, failureinfo, location, message,&
      & details)

    !> Instance
    class(serial_console_logger), intent(in) :: this

    !> Failure info to get the representation from
    type(failure_info), allocatable, intent(in) :: failureinfo

    !> Location string (unallocated if not available or not relevant)
    character(:), allocatable, intent(out) :: location

    !> Message string (unallocated if not available or not relevant)
    character(:), allocatable, intent(out) :: message

    !> Details string (unallocated if not available or not relevant)
    character(:), allocatable, intent(out) :: details

    character(:), allocatable :: buffer

    location = failureinfo%location%as_char()
    if (allocated(failureinfo%message)) message = failureinfo%message
    if (allocated(failureinfo%details)) details = failureinfo%details%as_char()

  end subroutine serial_console_logger_get_failure_info_repr

end module fortuno_serial_serialconlogger