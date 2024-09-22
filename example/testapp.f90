! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Test app with command line interface, collecting and executing the tests.
program testapp
  use fortuno_serial, only : execute_serial_cmd_app, test_list
  use test_fixtured, only : fixtured_tests => tests
  use test_parametrized, only : parametrized_tests => tests
  use test_simple, only : simple_tests => tests
  use test_fixtured_suite, only : fixtured_suite_tests => tests
  implicit none

  call execute_serial_cmd_app(test_list([&
      simple_tests(),&
      parametrized_tests(),&
      fixtured_tests(),&
      fixtured_suite_tests()&
  ]))

end program testapp
