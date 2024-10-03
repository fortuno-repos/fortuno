! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

#include "fortuno_serial.fpp"

!> Unit tests
module testapp_fpp_tests
  use fortuno_serial, only : is_equal, test => serial_case_item, test_list
  implicit none

contains


  subroutine test_success()
    CHECK(is_equal(1, 1))
  end subroutine test_success


  function tests()
    type(test_list) :: tests

    tests = test_list([&
        test("success", test_success)&
    ])

  end function tests

end module testapp_fpp_tests


!> Test app driving Fortuno unit tests
program testapp_fpp
  use fortuno_serial, only : execute_serial_cmd_app
  use testapp_fpp_tests, only : tests
  implicit none

  call execute_serial_cmd_app(tests())

end program testapp_fpp
