! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Test app with command line interface, collecting and executing the tests.
program testapp
  use fortuno_serial, only : execute_serial_cmd_app
  use simple_tests, only : simple_test_items
  use fixtured_module_tests, only : fixtured_module_test_items
  use fixtured_suite_tests, only : fixtured_suite_test_items
  use fixtured_tests, only : fixtured_test_items
  use parametrized_tests, only : parametrized_test_items
  implicit none

  call execute_serial_cmd_app(&
    testitems=[&
      simple_test_items(),&
      fixtured_test_items(),&
      parametrized_test_items(),&
      fixtured_suite_test_items(),&
      fixtured_module_test_items()&
    ]&
  )

end program testapp
