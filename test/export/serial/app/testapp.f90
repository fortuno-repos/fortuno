! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Unit tests
module testapp_serial_tests
  use fortuno_serial, only : is_equal, test => serial_case_item, check => serial_check, test_list
  implicit none

  private
  public :: tests

contains


  subroutine test_success()
    call check(is_equal(1, 1))
  end subroutine test_success


  function tests()
    type(test_list) :: tests

    tests = test_list([&
        test("success", test_success)&
    ])

  end function tests

end module testapp_serial_tests


!> Test app driving Fortuno unit tests
program testapp
  use testapp_serial_tests, only : tests
  use fortuno_serial, only : execute_serial_cmd_app
  implicit none

  call execute_serial_cmd_app(tests())

end program testapp
