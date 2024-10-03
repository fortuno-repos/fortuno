! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Test app with command line interface, collecting and executing the tests.
program testapp
  use fortuno_serial, only : execute_serial_cmd_app, test_list
  use test_fixtured_fpp, only : fixtured_fpp_tests => tests
  use test_parametrized_fpp, only : parametrized_fpp_tests => tests
  use test_simple_fpp, only : simple_fpp_tests => tests
  use test_fixtured_suite_fpp, only : fixtured_suite_fpp_tests => tests
  use test_fixtured_module_fpp, only : fixtured_module_fpp_tests => tests
  implicit none

  call execute_serial_cmd_app(test_list([&
      simple_fpp_tests(),&
      parametrized_fpp_tests(),&
      fixtured_fpp_tests(),&
      fixtured_suite_fpp_tests(),&
      fixtured_module_fpp_tests()&
  ]))

end program testapp
