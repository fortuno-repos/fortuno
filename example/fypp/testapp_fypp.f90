! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Test app with command line interface, collecting and executing the tests.
program testapp_fypp
  use fortuno_serial, only : execute_serial_cmd_app, test_list
  use test_fixtured_fypp, only : fixtured_fypp_tests => tests
  use test_parametrized_fypp, only : parametrized_fypp_tests => tests
  use test_simple_fypp, only : simple_fypp_tests => tests
  implicit none

  call execute_serial_cmd_app(test_list([&
      simple_fypp_tests(),&
      fixtured_fypp_tests(),&
      parametrized_fypp_tests()&
    ])&
  )

end program testapp_fypp
