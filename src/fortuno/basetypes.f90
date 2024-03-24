! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains base classes for itemizable test objects
module fortuno_basetypes
  implicit none

  private
  public :: test_base, test_case_base, test_suite_base
  public :: test_item, test_ptr_item


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
    class(test_base), allocatable :: item

  end type test_item


  !> Wrapped test_base class pointer for building arrays of generic test object pointers
  type :: test_ptr_item

    !> Actual test_base class pointer
    class(test_base), pointer :: item

  end type test_ptr_item


  !> Base class for all test cases
  type, extends(test_base), abstract :: test_case_base
  end type test_case_base


  !> Base class for all test suites containing test cases and other test suites
  type, extends(test_base), abstract :: test_suite_base

    !> Array of test_item objects
    type(test_item), allocatable :: items(:)

  end type test_suite_base

end module fortuno_basetypes
