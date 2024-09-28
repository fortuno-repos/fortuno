! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Unit tests
module testapp_coarray_tests
  use fortuno_coarray, only : context => coa_context, test => coa_pure_case_item,&
      & is_equal, test_list
  implicit none

contains


  function tests()
    type(test_list) :: tests

    tests = test_list([&
        test("success", test_success)&
    ])

  end function tests


  subroutine test_success(ctx)
    class(context), intent(inout) :: ctx

    integer :: buffer

    buffer = 0
    if (this_image() == 1) buffer = 1
    call co_broadcast(buffer, 1)
    call ctx%check(is_equal(buffer, 1))

  end subroutine test_success

end module testapp_coarray_tests


!> Test app driving Fortuno unit tests
program testapp
  use fortuno_coarray, only : execute_coa_cmd_app
  use testapp_coarray_tests, only : tests
  implicit none

  call execute_coa_cmd_app(tests())

end program testapp
