! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains context extended for the coarray case
module fortuno_coarray_coacontext
  use fortuno, only : context_factory, failure_location, test_context
  use fortuno_coarray_coaenv, only : coa_env
  use fortuno_coarray_coatestinfo, only : init_coa_failure_location, coa_failure_location
  implicit none

  private
  public :: as_coa_context, coa_context
  public :: init_coa_context_factory, coa_context_factory


  !> Extends the context with the coarray-specific data
  type, extends(test_context) :: coa_context
    private

    !> Main characteristics of the coa environment
    type(coa_env), public :: coaenv

    ! Mask of failed ranks
    integer, allocatable :: failedimages_(:)

  contains
    procedure :: check_logical => coa_context_check_logical
    procedure :: create_failure_location => coa_context_create_failure_location
  end type coa_context


  !> Factory to create coa context instances
  type, extends(context_factory) :: coa_context_factory
    type(coa_env) :: coaenv
  contains
    procedure :: create_context => coa_context_factory_create_context
  end type coa_context_factory

contains


  !> Returns an coa_context class pointer to a generic context pointer
  function as_coa_context(trg) result(ptr)

    !> Target to point at
    class(test_context), pointer, intent(in) :: trg

    !> Class specific pointer
    type(coa_context), pointer :: ptr

    select type (trg)
    type is (coa_context)
      ptr => trg
    class default
      error stop "Invalid test context type obtained in as_coa_context"
    end select

  end function as_coa_context


  !> Executes a check (using logical value as check result)
  subroutine coa_context_check_logical(this, cond, msg, file, line)

    !> Instance
    class(coa_context), intent(inout) :: this

    !> Whether check condition is fulfilled (check is successful)
    logical, intent(in) :: cond

    !> Check message
    character(*), optional, intent(in) :: msg

    !> Source file name
    character(*), optional, intent(in) :: file

    !> Line information
    integer, optional, intent(in) :: line

    logical, allocatable :: globalcond(:)
    integer :: ii

    allocate(globalcond(this%coaenv%nimages), source=.true.)
    globalcond(this%coaenv%image) = cond
    call this%coaenv%reduce_and(globalcond)
    this%failedimages_ = pack([(ii, ii = 1, size(globalcond))], .not. globalcond)
    call this%register_check(.not. all(globalcond), msg, file, line)

  end subroutine coa_context_check_logical


   !> Registers the location of the failure using the appropriate failure_location type
  subroutine coa_context_create_failure_location(this, failureloc, file, line)

    !> Instance
    class(coa_context), intent(inout) :: this

    !> Allocated and populated failure location on exit
    class(failure_location), allocatable, intent(out) :: failureloc

    !> File where failure occured
    character(*), optional, intent(in) :: file

    !> Line where failure occured
    integer, optional, intent(in) :: line

    type(coa_failure_location), allocatable :: coafailureloc

    allocate(coafailureloc)
    call init_coa_failure_location(coafailureloc, this%failedimages_, this%nchecks, file=file,&
          & line=line)
    call move_alloc(coafailureloc, failureloc)

  end subroutine coa_context_create_failure_location


  !> Initializes an coa_context_factory instance
  subroutine init_coa_context_factory(this, coaenv)

    !> Instance
    type(coa_context_factory), intent(out) :: this

    !> Communicator to use for the context
    type(coa_env), intent(in) :: coaenv

    integer :: ierror

    this%coaenv = coaenv

  end subroutine init_coa_context_factory


  !> Creates an coa context instance
  subroutine coa_context_factory_create_context(this, ctx)

    !> Instance
    class(coa_context_factory), intent(inout) :: this

    !> Created context on exit
    class(test_context), allocatable, intent(out) :: ctx

    type(coa_context), allocatable :: coactx

    allocate(coactx)
    coactx%coaenv = this%coaenv
    call move_alloc(coactx, ctx)

  end subroutine coa_context_factory_create_context

end module fortuno_coarray_coacontext
