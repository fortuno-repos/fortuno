! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains the simplest possible parameterless coarray test implementation
module fortuno_coarray_coapurecase
  use fortuno, only : test_item
  use fortuno_coarray_coabasetypes, only : coa_pure_case_base
  use fortuno_coarray_coacontext, only : coa_context
  implicit none

  private
  public :: coa_pure_case, coa_pure_case_item


  !> Coarray test case with
  type, extends(coa_pure_case_base) :: coa_pure_case

    !> Test procedure to call when test is run
    procedure(coa_pure_case_proc), nopass, pointer :: proc => null()

  contains

    !> Runs the test case
    procedure, public :: run => coa_pure_case_run

  end type coa_pure_case


  abstract interface

    !> Simple parameterless test procedure with explicitely passed context
    subroutine coa_pure_case_proc(ctx)
      import :: coa_context
      class(coa_context), intent(inout) :: ctx
    end subroutine coa_pure_case_proc

  end interface

contains


  !> Creates a pure coarray test case as a generic test item
  function coa_pure_case_item(name, proc) result(testitem)
    character(len=*), intent(in) :: name
    procedure(coa_pure_case_proc) :: proc
    type(test_item) :: testitem

    call testitem%init(coa_pure_case(name=name, proc=proc))

  end function coa_pure_case_item


  !> Runs the test case, by invoking the registered test procedure
  subroutine coa_pure_case_run(this, ctx)

    !> Instance
    class(coa_pure_case), intent(in) :: this

    !> Context to use
    class(coa_context), intent(inout) :: ctx

    call this%proc(ctx)

  end subroutine coa_pure_case_run

end module fortuno_coarray_coapurecase
