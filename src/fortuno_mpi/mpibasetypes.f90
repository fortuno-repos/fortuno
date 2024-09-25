! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains base type for mpi test suites
module fortuno_mpi_mpibasetypes
  use fortuno, only : test_case_base, test_suite_base
  implicit none

  private
  public :: as_mpi_case_base, mpi_case_base, mpi_case_base_run
  public :: as_mpi_suite_base, mpi_suite_base


  !> Base class for all mpi test cases
  type, extends(test_case_base), abstract :: mpi_case_base
  contains
    !> Procedure to be invoked by the driver to run the test
    procedure(mpi_case_base_run), deferred :: run
  end type mpi_case_base


  abstract interface

    subroutine mpi_case_base_run(this)
      import mpi_case_base
      implicit none
      class(mpi_case_base), intent(in) :: this
    end subroutine mpi_case_base_run

  end interface


  !> Base type for all mpi test suites
  type, extends(test_suite_base), abstract :: mpi_suite_base
  contains
    procedure :: set_up => mpi_suite_base_set_up
    procedure :: tear_down => mpi_suite_base_tear_down
  end type mpi_suite_base

contains


  !> Returns an mpi_context class pointer to a generic context pointer
  function as_mpi_case_base(trg) result(ptr)

    !> Target to point at
    class(test_case_base), pointer, intent(in) :: trg

    !> Class specific pointer
    class(mpi_case_base), pointer :: ptr

    select type (trg)
    class is (mpi_case_base)
      ptr => trg
    class default
      error stop "Invalid test context type obtained in as_mpi_case_base"
    end select

  end function as_mpi_case_base


  !> Returns an mpi_context class pointer to a generic context pointer
  function as_mpi_suite_base(trg) result(ptr)

    !> Target to point at
    class(test_suite_base), pointer, intent(in) :: trg

    !> Class specific pointer
    class(mpi_suite_base), pointer :: ptr

    select type (trg)
    class is (mpi_suite_base)
      ptr => trg
    class default
      error stop "Invalid test context type obtained in as_mpi_suite_base"
    end select

  end function as_mpi_suite_base


  !> Sets up the test suite, before the first test in the suite is run
  subroutine mpi_suite_base_set_up(this)

    !> Instance
    class(mpi_suite_base), intent(inout) :: this

  end subroutine mpi_suite_base_set_up


  !> Tears down the test suite, after the last test had been run
  subroutine mpi_suite_base_tear_down(this)

    !> Instance
    class(mpi_suite_base), intent(inout) :: this

  end subroutine mpi_suite_base_tear_down

end module fortuno_mpi_mpibasetypes
