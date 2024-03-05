! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Test app, collecting and executing the tests
program testapp
  use fortuno, only : execute_serial_cmd_app
  use simple_tests, only : get_simple_tests
  use fixtured_tests, only : get_fixtured_tests
  implicit none

  call execute_serial_cmd_app(&
    testitems=[&
      get_simple_tests(),&
      get_fixtured_tests()&
    ]&
  )

end program testapp
