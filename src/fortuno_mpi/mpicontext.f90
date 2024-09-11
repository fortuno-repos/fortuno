! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains context extended for the MPI-case
module fortuno_mpi_mpicontext
  use fortuno, only : context_factory, failure_location, test_context
  use fortuno_mpi_mpienv, only : mpi_env
  use fortuno_mpi_mpitestinfo, only : init_mpi_failure_location, mpi_failure_location
  implicit none

  private
  public :: as_mpi_context, mpi_context
  public :: init_mpi_context_factory, mpi_context_factory


  !> Extends the context with the MPI-specific data
  type, extends(test_context) :: mpi_context
    private

    !> Main characteristics of the MPI environment
    type(mpi_env), public :: mpienv

    ! Mask of failed ranks
    integer, allocatable :: failedranks_(:)

  contains
    procedure :: check_logical => mpi_context_check_logical
    procedure :: create_failure_location => mpi_context_create_failure_location
  end type mpi_context


  !> Factory to create mpi context instances
  type, extends(context_factory) :: mpi_context_factory
    type(mpi_env) :: mpienv
  contains
    procedure :: create_context => mpi_context_factory_create_context
  end type mpi_context_factory

contains


  !> Returns an mpi_context class pointer to a generic context pointer
  function as_mpi_context(trg) result(ptr)

    !> Target to point at
    class(test_context), pointer, intent(in) :: trg

    !> Class specific pointer
    type(mpi_context), pointer :: ptr

    select type (trg)
    type is (mpi_context)
      ptr => trg
    class default
      error stop "Invalid test context type obtained in as_mpi_context"
    end select

  end function as_mpi_context


  !> Executes a check (using logical value as check result)
  subroutine mpi_context_check_logical(this, cond, msg, file, line)

    !> Instance
    class(mpi_context), intent(inout) :: this

    !> Whether check condition is fulfilled (check is successful)
    logical, intent(in) :: cond

    !> Check message
    character(*), optional, intent(in) :: msg

    !> Source file name
    character(*), optional, intent(in) :: file

    !> Line information
    integer, optional, intent(in) :: line

    logical :: globalcond(this%mpienv%nranks)
    integer :: ii

    globalcond(:) = .true.
    ! Rank enumeration starts from zero, array position of a rank is shifted by one
    globalcond(this%mpienv%rank + 1) = cond
    call this%mpienv%reduce_and(globalcond)
    this%failedranks_ = pack([(ii - 1, ii = 1, size(globalcond))], .not. globalcond)
    call this%register_check(.not. all(globalcond), msg, file, line)

  end subroutine mpi_context_check_logical


   !> Registers the location of the failure using the appropriate failure_location type
  subroutine mpi_context_create_failure_location(this, failureloc, file, line)

    !> Instance
    class(mpi_context), intent(inout) :: this

    !> Allocated and populated failure location on exit
    class(failure_location), allocatable, intent(out) :: failureloc

    !> File where failure occured
    character(*), optional, intent(in) :: file

    !> Line where failure occured
    integer, optional, intent(in) :: line

    type(mpi_failure_location), allocatable :: mpifailureloc

    allocate(mpifailureloc)
    call init_mpi_failure_location(mpifailureloc, this%failedranks_, this%nchecks, file=file,&
          & line=line)
    call move_alloc(mpifailureloc, failureloc)

  end subroutine mpi_context_create_failure_location


  !> Initializes an mpi_context_factory instance
  subroutine init_mpi_context_factory(this, mpienv)

    !> Instance
    type(mpi_context_factory), intent(out) :: this

    !> Communicator to use for the context
    type(mpi_env), intent(in) :: mpienv

    integer :: ierror

    this%mpienv = mpienv

  end subroutine init_mpi_context_factory


  !> Creates an mpi context instance
  subroutine mpi_context_factory_create_context(this, ctx)

    !> Instance
    class(mpi_context_factory), intent(inout) :: this

    !> Created context on exit
    class(test_context), allocatable, intent(out) :: ctx

    type(mpi_context), allocatable :: mpictx

    allocate(mpictx)
    mpictx%mpienv = this%mpienv
    call move_alloc(mpictx, ctx)

  end subroutine mpi_context_factory_create_context

end module fortuno_mpi_mpicontext
