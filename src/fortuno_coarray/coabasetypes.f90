! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains base type for coarray test suites
module fortuno_coarray_coabasetypes
  use fortuno, only : test_case_base, test_suite_base
  use fortuno_coarray_coacontext, only : coa_context
  implicit none

  private
  public :: as_coa_pure_case_base, coa_pure_case_base, coa_pure_case_base_run
  public :: as_coa_pure_suite_base, coa_pure_suite_base


  !> Base class for all coarray test cases
  type, extends(test_case_base), abstract :: coa_pure_case_base
  contains
    !> Procedure to be invoked by the driver to run the test
    procedure(coa_pure_case_base_run), deferred :: run
  end type coa_pure_case_base


  abstract interface

    subroutine coa_pure_case_base_run(this, ctx)
      import :: coa_pure_case_base, coa_context
      implicit none
      class(coa_pure_case_base), intent(in) :: this
      class(coa_context), intent(inout) :: ctx
    end subroutine coa_pure_case_base_run

  end interface


  !> Base type for all coarray test suites
  type, extends(test_suite_base), abstract :: coa_pure_suite_base
  contains
    procedure :: set_up => coa_pure_suite_base_set_up
    procedure :: tear_down => coa_pure_suite_base_tear_down
  end type coa_pure_suite_base

contains


  !> Returns an coa_context class pointer to a generic context pointer
  function as_coa_pure_case_base(trg) result(ptr)

    !> Target to point at
    class(test_case_base), pointer, intent(in) :: trg

    !> Class specific pointer
    class(coa_pure_case_base), pointer :: ptr

    select type (trg)
    class is (coa_pure_case_base)
      ptr => trg
    class default
      error stop "Invalid test context type obtained in as_coa_pure_case_base"
    end select

  end function as_coa_pure_case_base


  !> Returns an coa_context class pointer to a generic context pointer
  function as_coa_pure_suite_base(trg) result(ptr)

    !> Target to point at
    class(test_suite_base), pointer, intent(in) :: trg

    !> Class specific pointer
    class(coa_pure_suite_base), pointer :: ptr

    select type (trg)
    class is (coa_pure_suite_base)
      ptr => trg
    class default
      error stop "Invalid test context type obtained in as_coa_pure_suite_base"
    end select

  end function as_coa_pure_suite_base


  !> Sets up the test suite, before the first test in the suite is run
  subroutine coa_pure_suite_base_set_up(this, ctx)

    !> Instance
    class(coa_pure_suite_base), intent(inout) :: this

    !> Context to use
    class(coa_context), intent(inout) :: ctx

  end subroutine coa_pure_suite_base_set_up


  !> Tears down the test suite, after the last test had been run
  subroutine coa_pure_suite_base_tear_down(this, ctx)

    !> Instance
    class(coa_pure_suite_base), intent(inout) :: this

    !> Context to use
    class(coa_context), intent(inout) :: ctx

  end subroutine coa_pure_suite_base_tear_down

end module fortuno_coarray_coabasetypes
