!> Test app driving Fortuno unit tests.
program testapp
  use fortuno, only : execute_serial_cmd_app, is_equal, test => serial_case_item,&
      check => serial_check
  implicit none

  call execute_serial_cmd_app(&
      testitems=[&
          test("success", test_success)&
      ]&
  )

contains

  subroutine test_success()
    call check(is_equal(1, 1))
  end subroutine test_success

end program testapp
