! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Unit tests
module testapp_mpi_tests
  use mpi_f08, only : mpi_bcast, MPI_INTEGER
  use fortuno_mpi, only : global_comm, is_equal, test => mpi_case_item, check => mpi_check,&
      & test_list, this_rank
  implicit none

contains

  subroutine test_success()

    integer :: buffer

    buffer = 0
    if (this_rank() == 0) buffer = 1
    call mpi_bcast(buffer, 1, MPI_INTEGER, 0, global_comm())
    call check(is_equal(buffer, 1))

  end subroutine test_success


  function tests()
    type(test_list) :: tests

    tests = test_list([&
        test("success", test_success)&
    ])

  end function tests

end module testapp_mpi_tests


!> Test app driving Fortuno unit tests
program testapp
  use fortuno_mpi, only : execute_mpi_cmd_app
  use testapp_mpi_tests, only : tests
  implicit none

  call execute_mpi_cmd_app(tests())

end program testapp
