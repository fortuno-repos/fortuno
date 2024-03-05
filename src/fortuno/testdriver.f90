! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Implements a generic test driver
module fortuno_testdriver
  use fortuno_basetypes, only : test_item, test_base
  use fortuno_basetypes, only : test_case_base, test_suite_base
  use fortuno_testcontext, only : context_factory, test_context
  use fortuno_testinfo, only : drive_result, init_drive_result, test_result, teststatus
  use fortuno_testlogger, only : test_logger, testtypes
  use fortuno_utils, only : basename, string
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


  !! Data item stored for each test (name and identifier).
  type :: test_data

    ! Unique integer identifier (path) of the test object
    integer, allocatable :: identifier(:)

    ! Fully qualified name of the test object
    character(:), allocatable :: name

    ! Test suite containing the current item (position in suitedatacont / suiteresults)
    integer, allocatable :: dependencies(:)

  end type test_data


  !! Minimalistic automatically growing array of test_data_ items.
  type :: test_data_container
    private
    type(test_data), pointer, public :: testdata(:) => null()
    type(test_data), pointer :: storage_(:) => null()
  contains
    procedure :: append => test_data_container_append
    final :: final_test_data_container
  end type test_data_container


  !> App for driving serial tests through command line app
  type :: test_driver
    private
    !> Result of a test drive, after run_tests() had been invoked
    class(test_runner), allocatable :: runner
    class(context_factory), allocatable :: ctxfactory
    class(test_logger), allocatable :: logger
    type(drive_result), public :: driveresult
    type(test_item), allocatable :: testitems(:)
    type(test_data_container) :: testdatacont
    type(test_data_container) :: suitedatacont
    integer, allocatable :: selectedsuites(:)
    integer, allocatable :: selectedtests(:)
  contains
    procedure :: register_tests => test_driver_register_tests
    procedure :: run_tests => test_driver_run_tests
    procedure :: get_test_names => test_driver_get_test_names
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


  !> Registers tests to consider
  subroutine test_driver_register_tests(this, testitems, selections)

    !> Instance
    class(test_driver), intent(inout) :: this

    !> Items to be considered by the app
    type(test_item), intent(in) :: testitems(:)

    !> Selection rule to constrain the testing only to a subset of the test items
    type(test_selection), optional, intent(in) :: selections(:)

    this%testitems = testitems
    call init_test_data_container(this%suitedatacont, 100)
    call init_test_data_container(this%testdatacont, 1000)
    call build_test_data_(this%testitems, "", [integer ::], [integer ::], this%testdatacont,&
        & this%suitedatacont)
    call get_selected_suites_and_tests_(this%suitedatacont%testdata, this%testdatacont%testdata,&
        & this%selectedsuites, this%selectedtests, selections)

  end subroutine test_driver_register_tests


  !> Runs the initialized app
  subroutine test_driver_run_tests(this, logger)

    !> Instance
    class(test_driver), intent(inout) :: this

    !> Logger for reporting events
    class(test_logger), intent(inout) :: logger

    call init_drive_result(this%driveresult, size(this%selectedsuites), size(this%selectedtests))

    call logger%start_drive()
    call logger%start_tests()
    call run_tests_(this%testitems, testtypes%suitesetup, this%suitedatacont%testdata,&
        & this%selectedsuites, this%driveresult%suiteresults(1, :), this%ctxfactory, this%runner,&
        & logger)
    call run_tests_(this%testitems, testtypes%testrun, this%testdatacont%testdata,&
        & this%selectedtests, this%driveresult%testresults, this%ctxfactory, this%runner, logger,&
        & this%driveresult%suiteresults(1, :))
    call run_tests_(this%testitems, testtypes%suiteteardown, this%suitedatacont%testdata,&
        & this%selectedsuites, this%driveresult%suiteresults(2, :), this%ctxfactory, this%runner,&
        & logger, this%driveresult%suiteresults(1, :))
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
    type(string), allocatable :: testnames(:)

    integer :: ntests, itest

    ntests = size(this%selectedtests)
    allocate(testnames(ntests))
    do itest = 1, ntests
      testnames(itest)%content = this%testdatacont%testdata(this%selectedtests(itest))%name
    end do

  end subroutine test_driver_get_test_names


  !! Executes various test objects.
  subroutine run_tests_(testitems, testtype, testdatas, selected, testresults, ctxfactory, runner,&
        & logger, dependencyresults)
    type(test_item), intent(inout) :: testitems(:)
    integer, intent(in) :: testtype
    type(test_data), intent(inout) :: testdatas(:)
    integer, intent(in) :: selected(:)
    type(test_result), target, intent(inout) :: testresults(:)
    class(context_factory), intent(inout) :: ctxfactory
    class(test_runner), intent(inout) :: runner
    class(test_logger), intent(inout) :: logger
    type(test_result), optional, target, intent(in) :: dependencyresults(:)

    type(test_result), pointer :: depresults(:)
    class(test_context), allocatable :: ctx
    character(:), allocatable :: repr
    integer :: depstatus
    integer :: iselect

    if (present(dependencyresults)) then
      depresults => dependencyresults
    else
      depresults => testresults
    end if

    do iselect = 1, size(selected)
      associate (testdata => testdatas(selected(iselect)), testresult => testresults(iselect))
        testresult%name = testdata%name
        testresult%dependencies = testdata%dependencies

        if (testtype == testtypes%suiteteardown) then
          depstatus = depresults(iselect)%status
        else if (size(testdata%dependencies) > 0) then
          depstatus = depresults(testdata%dependencies(1))%status
        else
          depstatus = teststatus%succeeded
        end if

        if (depstatus == teststatus%succeeded) then
          call ctxfactory%create_context(ctx)
          select case (testtype)
          case (testtypes%suitesetup)
            call initialize_finalize_suite_(testitems, testdata%identifier, .true., ctx, runner,&
                & repr)
          case (testtypes%testrun)
            call run_test_(testitems, testdata%identifier, ctx, runner, repr)
          case (testtypes%suiteteardown)
            call initialize_finalize_suite_(testitems, testdata%identifier, .false., ctx, runner,&
                & repr)
          end select
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

        select case (testtype)
        case (testtypes%suitesetup)
          call set_repr_name_(testresults, iselect, repr)
        case (testtypes%testrun)
          call set_repr_name_(testresults, iselect, repr, depresults)
        case (testtypes%suiteteardown)
          testresult%reprname = depresults(iselect)%reprname
        end select
        if (allocated(repr)) deallocate(repr)

        call logger%log_test_result(testtype, testresult)
      end associate
    end do

  end subroutine run_tests_


  !! Building up containers containing test object names and unique integer tuple identifiers.
  recursive subroutine build_test_data_(items, name, identifier, dependencies, testdatacont,&
      & suitedatacont)
    type(test_item), intent(in) :: items(:)
    character(*), intent(in) :: name
    integer, intent(in) :: identifier(:)
    integer, intent(in) :: dependencies(:)
    type(test_data_container), intent(inout) :: testdatacont
    type(test_data_container), intent(inout) :: suitedatacont

    integer :: ii
    character(:), allocatable :: newname
    integer, allocatable :: newidentifier(:), newdependencies(:)

    do ii = 1, size(items)
      newidentifier = [identifier, ii]
      associate (item => items(ii)%item)
        if (len(name) > 0) then
          newname = name // "/" // item%name
        else
          newname = item%name
        end if
        select type (item)
        class is (test_suite_base)
          call suitedatacont%append(test_data(newidentifier, newname, dependencies))
          ! The last element of the suitedata container is the current suite, the new dependency.
          newdependencies = [size(suitedatacont%testdata), dependencies]
          call build_test_data_(item%items, newname, newidentifier, newdependencies, testdatacont,&
              & suitedatacont)
        class is (test_case_base)
          call testdatacont%append(test_data(newidentifier, newname, dependencies))
        class default
          error stop  "Invalid test type obtained for test item '" // newname //&
              & "' (expected serial_suite_base or serial_case_base)"
        end select
      end associate
    end do
  end subroutine build_test_data_


  !! Initializes a test data container
  subroutine init_test_data_container(this, initsize)

    !> Instance
    type(test_data_container), intent(out) :: this

    !> Initial container size
    integer, intent(in) :: initsize

    allocate(this%storage_(100))
    ! Setting testdata pointer up, so that it has size 0.
    this%testdata => this%storage_(1:0)

  end subroutine init_test_data_container


  !! Finalizes a test data container-
  subroutine final_test_data_container(this)
    type(test_data_container), intent(inout) :: this

    if (associated(this%storage_)) deallocate(this%storage_)

  end subroutine final_test_data_container


  !! Append an item to the container and update the data pointer accordingly.
  subroutine test_data_container_append(this, testdata)
    class(test_data_container), intent(inout) :: this
    type(test_data), intent(in) :: testdata

    integer :: newsize
    type(test_data), pointer :: buffer(:)

    if (size(this%storage_) == size(this%testdata)) then
      newsize = int(size(this%storage_) * 1.4)
      allocate(buffer(newsize))
      buffer(1 : size(this%storage_)) = this%storage_
      deallocate(this%storage_)
      this%storage_ => buffer
      this%testdata => this%storage_(1 : size(this%testdata))
    end if
    this%storage_(size(this%testdata) + 1) = testdata
    this%testdata => this%storage_(1 : size(this%testdata) + 1)

  end subroutine test_data_container_append


  !! Run a test with a given identifier.
  recursive subroutine run_test_(testitems, identifier, ctx, runner, repr)
    type(test_item), target, intent(inout) :: testitems(:)
    integer, intent(in) :: identifier(:)
    class(test_context), target, intent(inout) :: ctx
    class(test_runner), intent(inout) :: runner
    character(:), allocatable, intent(out) :: repr

    associate (item => testitems(identifier(1))%item)
      block
        class(test_base), pointer :: scopeptr

        scopeptr => item
        call ctx%push_scope_ptr(scopeptr)
        if (size(identifier) == 1) then
          select type (item)
          class is (test_case_base)
            call runner%run_test(item, ctx)
            call item%get_as_char(repr)
          class default
            error stop "Internal error, unexpected test type in run_test_"
          end select
        else
          select type (item)
          class is (test_suite_base)
            call run_test_(item%items, identifier(2:), ctx, runner, repr)
          class default
            error stop "Internal error, unexpected test type in run_test_"
          end select
        end if
      end block
    end associate

  end subroutine run_test_


  !! Initialize of finalize a test suite with a given identifier.
  recursive subroutine initialize_finalize_suite_(testitems, identifier, init, ctx, runner, repr)
    type(test_item), target, intent(inout) :: testitems(:)
    integer, intent(in) :: identifier(:)
    logical, intent(in) :: init
    class(test_context), target, intent(inout) :: ctx
    class(test_runner), intent(inout) :: runner
    character(:), allocatable, intent(out) :: repr

    associate (item => testitems(identifier(1))%item)
      block
        class(test_base), pointer :: scopeptr

        scopeptr => item
        call ctx%push_scope_ptr(scopeptr)
        select type (item)
        class is (test_suite_base)
          if (size(identifier) == 1) then
            if (init) then
              call runner%set_up_suite(item, ctx)
              call item%get_as_char(repr)
            else
              call runner%tear_down_suite(item, ctx)
            end if
          else
            call initialize_finalize_suite_(item%items, identifier(2:), init, ctx, runner, repr)
          end if
        class default
          error stop "Internal error, unexpected test type in initialize_finalize_suite_"
        end select
      end block
    end associate

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
      if (allocated(repr)) name = name // "{" // repr // "}"
      if (size(testresult%dependencies) > 0) then
        testresult%reprname = depresults(testresult%dependencies(1))%reprname // "/" // name
      else
        testresult%reprname = name
      end if
    end associate

  end subroutine set_repr_name_


  subroutine get_selected_suites_and_tests_(suitedata, testdata, selectedsuites, selectedtests,&
        & selections)
    type(test_data), intent(in) :: suitedata(:), testdata(:)
    integer, allocatable, intent(out) :: selectedsuites(:), selectedtests(:)
    type(test_selection), optional, intent(in) :: selections(:)

    logical, allocatable :: testmask(:), suitemask(:)
    logical :: hasselection, selected, isincluded
    integer :: iselect, itest
    integer :: selectnamelen
    integer :: ii

    selectedsuites = [(ii, ii = 1, size(suitedata))]
    selectedtests = [(ii, ii = 1, size(testdata))]
    hasselection = present(selections)
    if (hasselection) hasselection = size(selections) > 0
    if (.not. hasselection) return

    allocate(testmask(size(testdata)))
    ! If first option is an exclusion, include all tests by default otherwise exclude them.
    testmask(:) = selections(1)%selectiontype == "-"
    do iselect = 1, size(selections)
      associate(selection => selections(iselect))
        isincluded = selection%selectiontype == "+"
        selectnamelen = len(selection%name)
        do itest = 1, size(testdata)
          associate (testdata => testdata(itest))
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

    allocate(suitemask(size(suitedata)), source=.false.)
    do itest = 1, size(testdata)
      if (testmask(itest)) suitemask(testdata(itest)%dependencies) = .true.
    end do

    selectedsuites = pack(selectedsuites, suitemask)
    selectedtests = pack(selectedtests, testmask)

  end subroutine get_selected_suites_and_tests_

end module fortuno_testdriver