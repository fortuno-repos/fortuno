! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains a serial logger implementation
module fortuno_serial_serialconlogger
  use fortuno_base_consolelogger, only : console_logger
  use fortuno_base_testinfo, only : failure_info
  implicit none

  private
  public :: serial_console_logger


  !> Serial logger
  type, extends(console_logger) :: serial_console_logger
  end type serial_console_logger

end module fortuno_serial_serialconlogger