! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Test app with command line interface, collecting and executing the tests.
program testapp
  use fortuno_serial, only : execute_serial_cmd_app, test_list
  use test_checkers, only : checkers_tests => tests
  implicit none

  call execute_serial_cmd_app(test_list([&
      checkers_tests()&
  ]))

end program testapp
