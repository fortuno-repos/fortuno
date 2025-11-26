! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Implements a generic test driver
module fortuno_testdriver
  use fortuno_basetypes, only : test_base, test_case_base, test_list, test_suite_base
  use fortuno_chartypes, only : stringable
  use fortuno_testcontext, only : context_factory, test_context
  use fortuno_testinfo, only : drive_result, init_drive_result, test_result, teststatus
  use fortuno_testlogger, only : test_logger, testtypes
  use fortuno_utils, only : basename, string_item
  implicit none

  private
  public :: init_test_driver, test_driver
  public :: test_runner
  public :: test_selection


  !> Implements the actual calls to the suite setups/teardowns and the test runs
  type, abstract :: test_runner
  contains
    procedure(test_runner_set_up_suite), deferred :: set_up_suite
    procedure(test_runner_tear_down_suite), deferred :: tear_down_suite
    procedure(test_runner_run_test), deferred :: run_test
  end type test_runner


  abstract interface

    !> Invokes the set-up method of a test suite
    subroutine test_runner_set_up_suite(this, testsuite, ctx)
      import :: test_context, test_runner, test_suite_base
      implicit none

      !> Instance
      class(test_runner), intent(in) :: this

      !> Test suite to set up
      class(test_suite_base), pointer, intent(in) :: testsuite

      !> Test context to use for the set-up
      class(test_context), pointer, intent(in) :: ctx

    end subroutine test_runner_set_up_suite


    !> Invokes the tear-down method of a test suite
    subroutine test_runner_tear_down_suite(this, testsuite, ctx)
      import :: test_context, test_runner, test_suite_base
      implicit none

      !> Instance
      class(test_runner), intent(in) :: this

      !> Test suite to tear down
      class(test_suite_base), pointer, intent(in) :: testsuite

      !> Test contect to use for the tear-down
      class(test_context), pointer, intent(in) :: ctx

    end subroutine test_runner_tear_down_suite


    !> Invokes the run method of a test case
    subroutine test_runner_run_test(this, test, ctx)
      import :: test_case_base, test_context, test_runner
      implicit none

      !> Instance
      class(test_runner), intent(in) :: this

      !> Test case to run
      class(test_case_base), pointer, intent(in) :: test

      !> Test context to use for the run
      class(test_context), pointer, intent(in) :: ctx

    end subroutine test_runner_run_test

  end interface


  !! Data item stored for each test when building a plain non-nested list of all test items.
  type :: test_data

    ! Unique integer identifier (path) of the test object
    integer, allocatable :: identifier(:)

    ! Fully qualified name of the test object
    character(:), allocatable :: name

    ! Test suite containing the current item (position in suitedatacont / suiteresults)
    integer, allocatable :: dependencies(:)

  end type test_data


  !! Wrapper type around test data (for efficiency and to circumvent GFortran compiler bugs)
  type :: test_data_ptr
    type(test_data), pointer :: ptr => null()
  contains
    final :: final_test_data_ptr
  end type test_data_ptr


  !! Minimalistic automatically growing array of test_data items.
  type :: test_data_container

    !! nr. of stored items
    integer :: nitems = 0

    !! the actual items, containing valid entries only for 1:nitems
    type(test_data_ptr), allocatable :: items(:)

  contains
    procedure :: add => test_data_container_add
  end type test_data_container


  !! Selection related reversible mapping data
  !!
  !! Forward mapping: selection index to test/suite index within container
  !! Reverse mapping: test/suite index within container to selection index
  !!
  type :: reversible_mapping
    integer, allocatable :: fwd(:), rev(:)
  end type reversible_mapping


  !> App for driving serial tests through command line app
  type :: test_driver
    private

    !> Result of a test drive, after run_tests() had been invoked
    type(drive_result), public :: driveresult

    class(test_runner), allocatable :: runner
    class(context_factory), allocatable :: ctxfactory
    class(test_logger), allocatable :: logger
    type(test_list) :: testlist
    type(test_data_container) :: suitedatacont, testdatacont
    type(reversible_mapping) :: suiteselection, testselection
  contains
    procedure :: register_tests => test_driver_register_tests
    procedure :: run_tests => test_driver_run_tests
    procedure :: get_test_names => test_driver_get_test_names
    final :: final_test_driver
  end type test_driver


  !> Represents a test selection
  type :: test_selection

    !> Name of the test to be selected
    character(:), allocatable :: name

    !> Type of the selection ("+": inclusion, "-": exclusion)
    character :: selectiontype = "+"

  end type

contains


  !> Initializes a test driver instance
  subroutine init_test_driver(this, ctxfactory, runner)

    !> Instance
    type(test_driver), intent(out) :: this

    !> Context factory to use for creating contextes
    class(context_factory), intent(in) :: ctxfactory

    !> Test runner for invoking test suite and test case methods
    class(test_runner), intent(in) :: runner

    this%ctxfactory = ctxfactory
    this%runner = runner

  end subroutine init_test_driver


  !> Finalizes the test driver (by freeing the allocated pointers in the test list)
  subroutine final_test_driver(this)
    type(test_driver), intent(inout) :: this

    call this%testlist%free()

  end subroutine final_test_driver


  !> Registers tests to consider
  subroutine test_driver_register_tests(this, testlist, selections)

    !> Instance
    class(test_driver), intent(inout) :: this

    !> Items to be considered by the app
    type(test_list), intent(in) :: testlist

    !> Selection rule to constrain the testing only to a subset of the test items
    type(test_selection), optional, intent(in) :: selections(:)

    this%testlist = testlist
    call init_test_data_container(this%suitedatacont, 100)
    call init_test_data_container(this%testdatacont, 5000)
    call build_test_data_(this%testlist, "", [integer ::], [integer ::], this%testdatacont,&
        & this%suitedatacont)
    call get_selected_suites_and_tests_(this%suitedatacont, this%testdatacont, this%suiteselection,&
        & this%testselection, selections)

  end subroutine test_driver_register_tests


  !> Runs the initialized app
  subroutine test_driver_run_tests(this, logger)

    !> Instance
    class(test_driver), intent(inout) :: this

    !> Logger for reporting events
    class(test_logger), intent(inout) :: logger

    call init_drive_result(this%driveresult, size(this%suiteselection%fwd),&
        & size(this%testselection%fwd))

    call logger%start_drive()
    call logger%start_tests()
    call run_suite_initializers_finalizers_(.true., this%testlist, this%suitedatacont,&
        & this%suiteselection, this%driveresult%suiteresults, this%ctxfactory, this%runner, logger)
    call run_tests_(this%testlist, this%suiteselection, this%driveresult%suiteresults(1, :),&
        & this%testdatacont, this%testselection, this%driveresult%testresults,&
        & this%ctxfactory, this%runner, logger)
    call run_suite_initializers_finalizers_(.false., this%testlist, this%suitedatacont,&
        this%suiteselection, this%driveresult%suiteresults, this%ctxfactory, this%runner, logger)
    call logger%end_tests()
    call this%driveresult%calculate_stats()
    call logger%log_drive_result(this%driveresult)
    call logger%end_drive()

  end subroutine test_driver_run_tests


  !> Returns the names of the registered tests
  subroutine test_driver_get_test_names(this, testnames)

    !> Instance
    class(test_driver), intent(in) :: this

    !> Name of all tests
    type(string_item), allocatable :: testnames(:)

    integer :: nselect, iselect

    nselect = size(this%testselection%fwd)
    allocate(testnames(nselect))
    do iselect = 1, nselect
      testnames(iselect)%value = this%testdatacont%items(this%testselection%fwd(iselect))%ptr%name
    end do

  end subroutine test_driver_get_test_names


  !! Runs the initializer or finalizer of each test suite
  subroutine run_suite_initializers_finalizers_(initializer, testlist, suitedatacont,&
      & suiteselection, suiteresults, ctxfactory, runner, logger)
    logical, intent(in) :: initializer
    type(test_list), intent(inout) :: testlist
    type(test_data_container), intent(inout) :: suitedatacont
    type(reversible_mapping), intent(in) :: suiteselection
    type(test_result), intent(inout) :: suiteresults(:,:)
    class(context_factory), intent(inout) :: ctxfactory
    class(test_runner), intent(inout) :: runner
    class(test_logger), intent(inout) :: logger

    class(test_context), allocatable :: ctx
    character(:), allocatable :: repr
    integer :: iselect, idata, depstatus, iresult, testtype

    if (initializer) then
      iresult = 1
      testtype = testtypes%suitesetup
    else
      iresult = 2
      testtype = testtypes%suiteteardown
    end if

    do iselect = 1, size(suiteselection%fwd)
      idata = suiteselection%fwd(iselect)
      associate (suitedata => suitedatacont%items(idata)%ptr, suiteresult => suiteresults(:, iselect))
        suiteresult(iresult)%name = suitedata%name
        ! Dependencies in result should point to entries in suite result array
        suiteresult(iresult)%dependencies = suiteselection%rev(suitedata%dependencies)

        if (initializer) then
          ! Initializer depends on the status of the initializaiton of the closest dependency.
          if (size(suiteresult(1)%dependencies) > 0) then
            depstatus = suiteresults(1, suiteresult(1)%dependencies(1))%status
          else
            depstatus = teststatus%succeeded
          end if
        else
          ! Finalizer depends on the status of the initializer of the same suite
          depstatus = suiteresult(1)%status
        end if

        if (depstatus == teststatus%succeeded) then
          call ctxfactory%create_context(ctx)
          call initialize_finalize_suite_(testlist, suitedata%identifier, initializer, ctx,&
              & runner, repr)
          suiteresult(iresult)%status = ctx%status()
          call ctx%pop_failure_info(suiteresult(iresult)%failureinfo)
          deallocate(ctx)
        else
          if (depstatus == teststatus%skipped) then
            suiteresult(iresult)%status = teststatus%skipped
          else
            suiteresult(iresult)%status = teststatus%ignored
          end if
        end if

        call set_repr_name_(suiteresults(iresult, :), iselect, repr)
        if (allocated(repr)) deallocate(repr)

        call logger%log_test_result(testtype, suiteresult(iresult))
      end associate
    end do

  end subroutine run_suite_initializers_finalizers_


  !! Runs all tests
  subroutine run_tests_(testlist, suiteselection, suiteinitresults, testdatacont, testselection,&
      & testresults, ctxfactory, runner, logger)
    type(test_list), intent(inout) :: testlist
    type(reversible_mapping), intent(in) :: suiteselection
    type(test_result), intent(in) :: suiteinitresults(:)
    type(test_data_container), intent(inout) :: testdatacont
    type(reversible_mapping), intent(in) :: testselection
    type(test_result), intent(inout) :: testresults(:)
    class(context_factory), intent(inout) :: ctxfactory
    class(test_runner), intent(inout) :: runner
    class(test_logger), intent(inout) :: logger

    class(test_context), allocatable :: ctx
    character(:), allocatable :: repr
    integer :: iselect, idata, depstatus

    do iselect = 1, size(testselection%fwd)
      idata = testselection%fwd(iselect)
      associate (testdata => testdatacont%items(idata)%ptr, testresult => testresults(iselect))
        testresult%name = testdata%name
        ! Dependencies in results should point to entries in suite result array
        testresult%dependencies = suiteselection%rev(testdata%dependencies)

        if (size(testresult%dependencies) > 0) then
          depstatus = suiteinitresults(testresult%dependencies(1))%status
        else
          depstatus = teststatus%succeeded
        end if
        if (depstatus == teststatus%succeeded) then
          call ctxfactory%create_context(ctx)
          call run_test_(testlist, testdata%identifier, ctx, runner, repr)
          testresult%status = ctx%status()
          call ctx%pop_failure_info(testresult%failureinfo)
          deallocate(ctx)
        else
          if (depstatus == teststatus%skipped) then
            testresult%status = teststatus%skipped
          else
            testresult%status = teststatus%ignored
          end if
        end if

        call set_repr_name_(testresults, iselect, repr, suiteinitresults)
        if (allocated(repr)) deallocate(repr)

        call logger%log_test_result(testtypes%testrun, testresult)
      end associate
    end do

  end subroutine run_tests_


  !! Building up containers containing test object names and unique integer tuple identifiers.
  recursive subroutine build_test_data_(items, name, identifier, dependencies, testdatacont,&
      & suitedatacont)
    type(test_list), intent(in) :: items
    character(*), intent(in) :: name
    integer, intent(in) :: identifier(:)
    integer, intent(in) :: dependencies(:)
    type(test_data_container), intent(inout) :: testdatacont
    type(test_data_container), intent(inout) :: suitedatacont

    type(test_data) :: testdata
    class(test_base), pointer :: item
    integer :: ii
    character(:), allocatable :: newname
    integer, allocatable :: newidentifier(:), newdependencies(:)

    do ii = 1, items%size()
      newidentifier = [identifier, ii]
      item => items%view(ii)
      if (len(name) > 0) then
        newname = name // "/" // item%name
      else
        newname = item%name
      end if
      testdata = test_data(newidentifier, newname, dependencies)
      select type (item)
      class is (test_suite_base)
        call suitedatacont%add(testdata)
        ! The last element of the suitedata container is the current suite, the new dependency.
        newdependencies = [suitedatacont%nitems, dependencies]
        call build_test_data_(item%tests, newname, newidentifier, newdependencies, testdatacont,&
            & suitedatacont)
      class is (test_case_base)
        call testdatacont%add(testdata)
      class default
        error stop  "Invalid test type obtained for test item '" // newname //&
            & "' (expected serial_suite_base or serial_case_base)"
      end select
    end do
  end subroutine build_test_data_


  !! Initializes a test data container
  subroutine init_test_data_container(this, initsize)

    !> Instance
    type(test_data_container), intent(out) :: this

    !> Initial container size
    integer, intent(in) :: initsize

    allocate(this%items(initsize))
    this%nitems = 0

  end subroutine init_test_data_container


  !! Finalizes a test data pointer
  elemental subroutine final_test_data_ptr(this)
    type(test_data_ptr), intent(inout) :: this

    if (associated(this%ptr)) deallocate(this%ptr)

  end subroutine final_test_data_ptr


  !! Appends an item to the container.
  subroutine test_data_container_add(this, testdata)
    class(test_data_container), intent(inout) :: this
    type(test_data), intent(in) :: testdata

    integer :: newsize
    type(test_data_ptr), allocatable :: buffer(:)

    if (size(this%items) == this%nitems) then
      call move_alloc(this%items, buffer)
      newsize = max(int(this%nitems * 1.3), this%nitems + 10)
      allocate(this%items(newsize))
      this%items(1 : this%nitems) = buffer
    end if
    allocate(this%items(this%nitems + 1)%ptr, source=testdata)
    this%nitems = this%nitems + 1

  end subroutine test_data_container_add


  !! Runs a test with a given identifier.
  recursive subroutine run_test_(testlist, identifier, ctx, runner, repr)
    type(test_list), intent(inout) :: testlist
    integer, intent(in) :: identifier(:)
    class(test_context), target, intent(inout) :: ctx
    class(test_runner), intent(inout) :: runner
    character(:), allocatable, intent(out) :: repr

    class(test_base), pointer :: scopeptr, item
    class(stringable), allocatable :: state

    scopeptr => testlist%view(identifier(1))
    call ctx%push_scope_ptr(scopeptr)
    if (size(identifier) == 1) then
      item => testlist%view(identifier(1))
      select type (item)
      class is (test_case_base)
        call runner%run_test(item, ctx)
        call ctx%pop_state(state)
        if (allocated(state)) repr = state%as_string()
      class default
        error stop "Internal error, unexpected test type in run_test_"
      end select
    else
      item => testlist%view(identifier(1))
      select type (item)
      class is (test_suite_base)
        call run_test_(item%tests, identifier(2:), ctx, runner, repr)
      class default
        error stop "Internal error, unexpected test type in run_test_"
      end select
    end if

  end subroutine run_test_


  !! Initialize of finalize a test suite with a given identifier.
  recursive subroutine initialize_finalize_suite_(testlist, identifier, init, ctx, runner, repr)
    type(test_list), intent(inout) :: testlist
    integer, intent(in) :: identifier(:)
    logical, intent(in) :: init
    class(test_context), target, intent(inout) :: ctx
    class(test_runner), intent(inout) :: runner
    character(:), allocatable, intent(out) :: repr

    class(test_base), pointer :: scopeptr, item
    class(stringable), allocatable :: state

    scopeptr => testlist%view(identifier(1))
    call ctx%push_scope_ptr(scopeptr)
    item => testlist%view(identifier(1))
    select type (item)
    class is (test_suite_base)
      if (size(identifier) == 1) then
        if (init) then
          call runner%set_up_suite(item, ctx)
          call ctx%pop_state(state)
          if (allocated(state)) repr = state%as_string()
        else
          call runner%tear_down_suite(item, ctx)
        end if
      else
        call initialize_finalize_suite_(item%tests, identifier(2:), init, ctx, runner, repr)
      end if
    class default
      error stop "Internal error, unexpected test type in initialize_finalize_suite_"
    end select

  end subroutine initialize_finalize_suite_


  !! Sets the reprname field of a given test result
  subroutine set_repr_name_(testresults, ind, repr, dependencyresults)
    type(test_result), target, intent(inout) :: testresults(:)
    integer, intent(in) :: ind
    character(:), allocatable, intent(in) :: repr
    type(test_result), target, optional, intent(in) :: dependencyresults(:)

    type(test_result), pointer :: depresults(:)
    character(:), allocatable :: name

    if (present(dependencyresults)) then
      depresults => dependencyresults
    else
      depresults => testresults
    end if

    associate (testresult => testresults(ind))
      name = basename(testresult%name)
      if (allocated(repr)) name = name // " {" // repr // "}"
      if (size(testresult%dependencies) > 0) then
        testresult%reprname = depresults(testresult%dependencies(1))%reprname // "/" // name
      else
        testresult%reprname = name
      end if
    end associate

  end subroutine set_repr_name_


  !! Returns indices of selected suites and tests.
  subroutine get_selected_suites_and_tests_(suitedatacont, testdatacont, suiteselection,&
        & testselection, selections)
    type(test_data_container), intent(in) :: suitedatacont, testdatacont
    type(reversible_mapping), intent(out) :: suiteselection, testselection
    type(test_selection), optional, intent(in) :: selections(:)

    logical, allocatable :: testmask(:), suitemask(:)
    logical :: hasselection, selected, isincluded
    integer :: iselect, itest
    integer :: selectnamelen
    integer :: ii

    hasselection = present(selections)
    if (hasselection) hasselection = size(selections) > 0
    if (.not. hasselection) then
      suiteselection%fwd = [(ii, ii = 1, suitedatacont%nitems)]
      suiteselection%rev = suiteselection%fwd
      testselection%fwd = [(ii, ii = 1, testdatacont%nitems)]
      testselection%rev = testselection%fwd
      return
    end if

    allocate(testmask(testdatacont%nitems))
    ! If first option is an exclusion, include all tests by default otherwise exclude them.
    testmask(:) = selections(1)%selectiontype == "-"
    do iselect = 1, size(selections)
      associate(selection => selections(iselect))
        isincluded = selection%selectiontype == "+"
        selectnamelen = len(selection%name)
        do itest = 1, testdatacont%nitems
          associate (testdata => testdatacont%items(itest)%ptr)
            if (len(testdata%name) == selectnamelen) then
              selected = testdata%name == selection%name
            else if (len(testdata%name) > selectnamelen) then
              selected = testdata%name(:selectnamelen) == selection%name&
                  & .and. testdata%name(selectnamelen + 1 : selectnamelen + 1) == "/"
            else
              selected = .false.
            end if
            if (selected) testmask(itest) = isincluded
          end associate
        end do
      end associate
    end do

    allocate(suitemask(suitedatacont%nitems), source=.false.)
    do itest = 1, testdatacont%nitems
      if (testmask(itest)) suitemask(testdatacont%items(itest)%ptr%dependencies) = .true.
    end do

    call get_rev_map_from_mask_(suitemask, suiteselection)
    call get_rev_map_from_mask_(testmask, testselection)

  end subroutine get_selected_suites_and_tests_


  !! Creates a forward/backward map based on a (selection) mask.
  subroutine get_rev_map_from_mask_(mask, mapping)
    logical, intent(in) :: mask(:)
    type(reversible_mapping), intent(out) :: mapping

    integer :: ii

    mapping%fwd = pack([(ii, ii = 1, size(mask))], mask)
    allocate(mapping%rev(size(mask)), source=0)
    do ii = 1, size(mapping%fwd)
      mapping%rev(mapping%fwd(ii)) = ii
    end do

  end subroutine get_rev_map_from_mask_

end module fortuno_testdriver
