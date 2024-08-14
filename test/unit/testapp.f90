! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Test app with command line interface, collecting and executing the tests.
program testapp
  use fortuno_serial, only : execute_serial_cmd_app
  use test_checkers, only : checkers_test_items
  implicit none

  call execute_serial_cmd_app(&
      testitems=[&
          checkers_test_items()&
      ]&
  )

end program testapp
