! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

#include "fortuno_serial.fpp"

!> Unit tests
module testapp_fpp_tests
  use fortuno_serial, only : is_equal, test => serial_case_item, test_item
  implicit none

  private
  public :: test_items

contains


  subroutine test_success()
    CHECK(is_equal(1, 1))
  end subroutine test_success


  function test_items() result(testitems)
    type(test_item), allocatable :: testitems(:)

    testitems = [&
        test("success", test_success)&
    ]

  end function test_items

end module testapp_fpp_tests


!> Test app driving Fortuno unit tests
program testapp_fpp
  use testapp_fpp_tests, only : test_items
  use fortuno_serial, only : execute_serial_cmd_app
  implicit none

  call execute_serial_cmd_app(testitems=test_items())

end program testapp_fpp
