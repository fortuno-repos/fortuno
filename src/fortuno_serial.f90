! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Interface module for the Fortuno testing framework with the serial interface.
module fortuno_serial
  use fortuno
  use fortuno_serial_serialbasetypes, only : serial_case_base, serial_suite_base
  use fortuno_serial_serialcmdapp, only : execute_serial_cmd_app, init_serial_cmd_app,&
      & serial_cmd_app
  use fortuno_serial_serialglobalctx, only : serial_check, serial_check_failed, serial_failed,&
      & serial_scope_pointers, serial_skip, serial_skipped, serial_store_state
  use fortuno_serial_serialcase, only : serial_case, serial_case_item
  use fortuno_serial_serialsuite, only : serial_suite, serial_suite_item
  implicit none

end module fortuno_serial
