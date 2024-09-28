! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains the simplest possible parameterless serial test implementation
module fortuno_mpi_mpicase
  use fortuno, only : test_item
  use fortuno_mpi_mpibasetypes, only : mpi_case_base
  implicit none

  private
  public :: mpi_case, mpi_case_item


  !> Serial test case with simple (parameterless) test procedure
  type, extends(mpi_case_base) :: mpi_case

    !> Test procedure to call when test is run
    procedure(mpi_case_proc), nopass, pointer :: proc => null()

  contains

    !> Runs the test case
    procedure, public :: run => mpi_case_run

  end type mpi_case


  abstract interface

    !> Simple parameterless test procedure
    subroutine mpi_case_proc()
    end subroutine mpi_case_proc

  end interface

contains


  !> Creates a serial test case as a generic test item
  function mpi_case_item(name, proc) result(testitem)
    character(len=*), intent(in) :: name
    procedure(mpi_case_proc) :: proc
    type(test_item) :: testitem

    call testitem%init(mpi_case(name=name, proc=proc))

  end function mpi_case_item


  !> Runs the test case, by invoking the registered test procedure
  subroutine mpi_case_run(this)

    !> Instance
    class(mpi_case), intent(in) :: this

    call this%proc()

  end subroutine mpi_case_run

end module fortuno_mpi_mpicase
