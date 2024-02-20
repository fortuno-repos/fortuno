! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains the base context definition.
module fortuno_testcontext
  use fortuno_basetypes, only : test_base, test_ptr_item
  use fortuno_testinfo, only : check_result, failure_info, failure_location, init_failure_location,&
      & teststatus
  implicit none

  private
  public :: context_factory, test_context


  !> Base of all test contexts, to be extended by driver specific ones (e.g. serial, mpi, etc.).
  type :: test_context
    private

    !> nr. of checks executed so far (needed for enumerating checks if file/line info not available)
    integer, public :: nchecks = 0

    !> info about check failures in current context
    type(failure_info), allocatable :: failureinfo_

    !> status of the context
    integer :: status_ = teststatus%succeeded

    ! whether last check failed
    logical :: checkfailed_ = .false.

    ! nr. of suite pointers stored so far
    integer :: nscopes_ = 0

    ! buffer for storing the scopes which contain the current item
    type(test_ptr_item), allocatable :: scopebuffer_(:)

  contains
    procedure :: check_logical => test_context_check_logical
    procedure :: check_check_result => test_context_check_check_result
    generic :: check => check_logical, check_check_result
    procedure :: register_check => test_context_register_check
    procedure :: check_failed => test_context_check_failed
    procedure :: failed => test_context_failed
    procedure :: skip => test_context_skip
    procedure :: status => test_context_status
    procedure :: pop_failure_info => test_context_pop_failure_info
    procedure :: push_scope_ptr => test_context_push_scope_ptr
    procedure :: scope_pointers => test_context_scope_pointers
    procedure :: create_failure_location => test_context_create_failure_location
  end type test_context


  !> Factory to produce a test context class instance
  type, abstract :: context_factory
  contains
    procedure(context_factory_create_context), deferred :: create_context
  end type context_factory


  abstract interface

    subroutine context_factory_create_context(this, ctx)
      import :: context_factory, test_context
      implicit none

      !> instance
      class(context_factory), intent(inout) :: this

      !> test context class instance on exit
      class(test_context), allocatable, intent(out) :: ctx

    end subroutine context_factory_create_context

  end interface

contains

  !> Executes a check (using logical value as check result).
  subroutine test_context_check_logical(this, cond, msg, file, line)

    !> instance
    class(test_context), intent(inout) :: this

    !> whether check condition is fulfilled (check is successful)
    logical, intent(in) :: cond

    !> check message
    character(*), optional, intent(in) :: msg

    !> source file name
    character(*), optional, intent(in) :: file

    !> line information
    integer, optional, intent(in) :: line

    call this%register_check(.not. cond, msg=msg, file=file, line=line)

  end subroutine test_context_check_logical


  !> Registers a check in the context, should be called from check() with the result of the check.
  subroutine test_context_register_check(this, checkfailed, msg, file, line)

    !> instance
    class(test_context), intent(inout) :: this

    !> whether check has failed
    logical, intent(in) :: checkfailed

    !> check message
    character(*), optional, intent(in) :: msg

    !> source file name
    character(*), optional, intent(in) :: file

    !> line information
    integer, optional, intent(in) :: line

    type(failure_info), allocatable :: failureinfo

    this%nchecks = this%nchecks + 1
    this%checkfailed_ = checkfailed

    ! Not recording failure if check was successful or test is in status 'skipped'
    if (.not. this%checkfailed_ .or. this%status_ == teststatus%skipped) return

    this%status_ = teststatus%failed
    allocate(failureinfo)
    call this%create_failure_location(failureinfo%location, file, line)
    if (present(msg)) failureinfo%message = msg
    if (allocated(this%failureinfo_)) call move_alloc(this%failureinfo_, failureinfo%previous)
    call move_alloc(failureinfo, this%failureinfo_)

  end subroutine test_context_register_check


  !> Exectutes a check (using detailed check result information)
  subroutine test_context_check_check_result(this, checkresult, msg, file, line)

    !> instance
    class(test_context), intent(inout) :: this

    !> whether check condition is fulfilled (check is successful)
    type(check_result), intent(in) :: checkresult

    !> check message
    character(*), optional, intent(in) :: msg

    !> source file name
    character(*), optional, intent(in) :: file

    !> line information
    integer, optional, intent(in) :: line

    call this%check_logical(checkresult%success, msg, file, line)
    ! Test might already have the status 'skipped', do not record failure in that case.
    if (.not. (this%check_failed() .and. this%failed())) return
    if (allocated(checkresult%details)) this%failureinfo_%details = checkresult%details

  end subroutine test_context_check_check_result


  !> Whether test has failed already (at some previous check).
  pure function test_context_failed(this) result(failed)

    !> instance
    class(test_context), intent(in) :: this

    !> whether test has failed already
    logical :: failed

    failed = (this%status_ == teststatus%failed)

  end function test_context_failed


  !> Whether the last check has failed.
  pure function test_context_check_failed(this) result(failed)

    !> instance
    class(test_context), intent(in) :: this

    !> whether last check has failed
    logical :: failed

    failed = this%checkfailed_

  end function test_context_check_failed


  !> Current test status.
  pure function test_context_status(this) result(status)

    !> instance
    class(test_context), intent(in) :: this

    !> test status
    integer :: status

    status = this%status_

  end function test_context_status


  !> Sets the test status to skipped (provided no failure occured so far).
  subroutine test_context_skip(this)

    !> instance
    class(test_context), intent(inout) :: this

    if (this%status_ == teststatus%succeeded) this%status_ = teststatus%skipped

  end subroutine test_context_skip


  !> Pops the failure info from the context.
  subroutine test_context_pop_failure_info(this, failureinfo)

    !> instance
    class(test_context), intent(inout) :: this

    !> extracted failure info (or unallocated, if no failure info was present)
    type(failure_info), allocatable, intent(out) :: failureinfo

    if (allocated(this%failureinfo_)) call move_alloc(this%failureinfo_, failureinfo)

  end subroutine test_context_pop_failure_info


  !> Pushes a suite pointer to the context.
  subroutine test_context_push_scope_ptr(this, scopeptr)

    !> instance
    class(test_context), intent(inout) :: this

    !> pointer to the suite to be pushed
    class(test_base), pointer, intent(in) :: scopeptr

    type(test_ptr_item), allocatable :: tmp(:)

    if (this%nscopes_ == 0) then
      allocate(this%scopebuffer_(10))
      this%scopebuffer_(1)%item => scopeptr
      this%nscopes_ = 1
      return
    end if
    if (this%nscopes_ == size(this%scopebuffer_)) then
      allocate(tmp(int(this%nscopes_ * 1.4)))
      tmp(1:this%nscopes_) = this%scopebuffer_(1:this%nscopes_)
      call move_alloc(tmp, this%scopebuffer_)
    end if
    this%nscopes_ = this%nscopes_ + 1
    this%scopebuffer_(this%nscopes_)%item => scopeptr

  end subroutine test_context_push_scope_ptr


  !> Returns the suite pointers stored in the context.
  function test_context_scope_pointers(this) result(scopeptrs)

    !> instance
    class(test_context), intent(in) :: this

    !> pointers to the suites enclosing the current item
    type(test_ptr_item), allocatable :: scopeptrs(:)

    if (this%nscopes_ == 0) then
      allocate(scopeptrs(0))
    else
      scopeptrs = this%scopebuffer_(this%nscopes_ : 1 : -1)
    end if

  end function test_context_scope_pointers


  !> Creates the location of the failure using the appropriate failure_location type
  subroutine test_context_create_failure_location(this, failureloc, file, line)

    !> instance
    class(test_context), intent(inout) :: this

    !> allocated and populated failure location on exit
    class(failure_location), allocatable, intent(out) :: failureloc

    !> file where failure occured
    character(*), optional, intent(in) :: file

    !> line where failure occured
    integer, optional, intent(in) :: line

    allocate(failure_location :: failureloc)
    select type (failureloc)
    type is (failure_location)
      call init_failure_location(failureloc, this%nchecks, file=file, line=line)
    end select

  end subroutine test_context_create_failure_location

end module fortuno_testcontext
