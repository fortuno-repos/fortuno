! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains base classes for itemizable test objects
module fortuno_basetypes
  implicit none

  private
  public :: test_base, test_case_base, test_suite_base
  public :: test_item, test_ptr_item
  public :: test_list


  !> Base class for all test objects
  !!
  !! Represents a generic test object, which can be a test case or a test suite.
  !!
  type, abstract :: test_base

    !> Name of the generic test
    character(:), allocatable :: name

  end type test_base


  !> Wrapped test_base class instance for building arrays of generic test object instances
  type :: test_item

    !> Actual test_base class instance
    class(test_base), pointer :: item => null()

  contains

    procedure :: init => test_item_init

  end type test_item

  !> Structure constructor for test_item
  interface test_item
    module procedure new_test_item
  end interface test_item


  !> Wrapped test_base class pointer for building arrays of generic test object view pointers
  type :: test_ptr_item

    !> Actual test_base class pointer
    class(test_base), pointer :: item => null()

  end type test_ptr_item


  !> A list of test_base instances
  type :: test_list
    private
    type(test_ptr_item), pointer :: storage_(:) => null()
    integer :: nitems = 0
  contains
    procedure :: size => test_list_size
    procedure :: view => test_list_view
    procedure :: free => test_list_free
    procedure, private :: ensure_storage_size_ => test_list_ensure_storage_size_
  end type


  !> Structure constructor for test_list
  interface test_list
    module procedure new_test_list_from_items, new_test_list_from_lists
  end interface test_list


  !> Base class for all test cases
  type, extends(test_base), abstract :: test_case_base
  end type test_case_base


  !> Base class for all test suites containing test cases and other test suites
  type, extends(test_base), abstract :: test_suite_base

    !> List of tests objects (test_cases and test suites) the suite contains
    type(test_list) :: tests

  end type test_suite_base

contains

  !> Initializes a test item with the copy of a test_base instance.
  !!
  !! Note: This should be done only once for a test item instance. If the instance has already a
  !! content, it will stop with an error.
  !!
  subroutine test_item_init(this, test)

    !> Instance
    class(test_item), intent(inout) :: this

    !> Test to copy and store as test_item
    class(test_base), intent(in) :: test

    if (associated(this%item)) error stop "Double initialization of a test_item instance"
    allocate(this%item, source=test)

  end subroutine test_item_init


  !> Returns a test item with the copy a test_base instance.
  function new_test_item(test) result(this)

    !> Test to copy and store as test_item
    class(test_base), intent(in) :: test

    !> Initialized instance on return
    type(test_item) :: this

    call this%init(test)

  end function new_test_item


  !> Creates a new test_list from an array of test_item instances.
  function new_test_list_from_items(testitems) result(this)

    !> Test item instances to store in the test_list
    type(test_item), intent(in) :: testitems(:)

    !> Initialized instance on return
    type(test_list) :: this

    integer :: ii

    call this%ensure_storage_size_(size(testitems))
    do ii = 1, size(testitems)
      this%storage_(ii)%item => testitems(ii)%item
    end do
    this%nitems = size(testitems)

  end function new_test_list_from_items


  !> Creates a new test_list from an array of test_lists
  function new_test_list_from_lists(testlists) result(this)

    !> Array of test_lists
    type(test_list), intent(in) :: testlists(:)

    !> Initialized instance on return
    type(test_list) :: this

    integer :: totalsize
    integer :: ilist, iitem

    totalsize = sum(testlists%size())
    call this%ensure_storage_size_(totalsize)
    do ilist = 1, size(testlists)
      associate (list => testlists(ilist))
        do iitem = 1, list%nitems
          this%storage_(this%nitems + iitem)%item => list%storage_(iitem)%item
        end do
        this%nitems = this%nitems + list%nitems
      end associate
    end do

  end function new_test_list_from_lists


  !> Returns the size of the list
  pure elemental function test_list_size(this) result(listsize)

    !> Instance
    class(test_list), intent(in) :: this

    !> Nr. of elements in the list
    integer :: listsize

    listsize = this%nitems

  end function test_list_size


  !> Returns a pointer to a given element of the list
  function test_list_view(this, ind) result(itemptr)

    !> Instance
    class(test_list), intent(in) :: this

    !> Element to get a view to
    integer, intent(in) :: ind

    !> Pointer to the given test_base instance
    class(test_base), pointer :: itemptr

    itemptr => this%storage_(ind)%item

  end function test_list_view


  !> Recursively frees all elements contained in the list
  recursive subroutine test_list_free(this)
    class(test_list), intent(inout) :: this

    integer :: ii

    if (.not. associated(this%storage_)) return
    do ii = 1, this%nitems
      select type (item => this%storage_(ii)%item)
      class is (test_suite_base)
        call item%tests%free()
      end select
      deallocate(this%storage_(ii)%item)
    end do
    deallocate(this%storage_)
    this%nitems = 0

  end subroutine test_list_free


  !! Ensures that the test_list has enough storage for a given size
  subroutine test_list_ensure_storage_size_(this, newsize)
    class(test_list), intent(inout) :: this
    integer, intent(in) :: newsize

    type(test_ptr_item), pointer :: buffer(:)
    integer :: storagesize

    if (associated(this%storage_)) then
      storagesize = size(this%storage_)
    else
      storagesize = 0
    end if
    if (newsize <= storagesize) return

    if (associated(this%storage_)) then
      buffer => this%storage_
    else
      buffer => null()
    end if
    storagesize = max(newsize, storagesize + 10, int(real(storagesize) * 1.3))
    allocate(this%storage_(storagesize))
    if (associated(buffer)) this%storage_(1:this%nitems) = buffer(1:this%nitems)

  end subroutine test_list_ensure_storage_size_

end module fortuno_basetypes
