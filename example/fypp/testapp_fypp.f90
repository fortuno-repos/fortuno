! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Test app with command line interface, collecting and executing the tests.
program testapp_fypp
  use fortuno_serial, only : execute_serial_cmd_app
  use simple_fypp_tests, only : simple_fypp_test_items
  use fixtured_fypp_tests, only : fixtured_fypp_test_items
  use parametrized_fypp_tests, only : parametrized_fypp_test_items
  implicit none

  call execute_serial_cmd_app(&
    testitems=[&
      simple_fypp_test_items(),&
      fixtured_fypp_test_items(),&
      parametrized_fypp_test_items()&
    ]&
  )

end program testapp_fypp
