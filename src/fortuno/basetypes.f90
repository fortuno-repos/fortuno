! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains base classes for itemizable test objects
module fortuno_basetypes
  implicit none

  private
  public :: char_repr
  public :: test_base, test_case_base, test_suite_base
  public :: test_item, test_ptr_item

  !> Character representable object
  type, abstract :: char_repr
  contains
    procedure(char_repr_as_char), deferred :: as_char
  end type char_repr

  abstract interface

    !> Returns the character representation of a character representable object.
    function char_repr_as_char(this) result(charrepr)
      import :: char_repr
      implicit none

      !> Instance
      class(char_repr), intent(in) :: this

      !> Character representation of the object.
      character(:), allocatable :: charrepr

    end function char_repr_as_char

  end interface


  !> Base class for all test objects
  !!
  !! Represents a generic test object, which can be a test case or a test suite.
  !!
  type, abstract :: test_base

    !> Name of the generic test
    character(:), allocatable :: name

    !> Character representable internal state
    class(char_repr), allocatable :: state

  contains

    procedure :: get_as_char => test_base_get_as_char

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

contains


  !> Delivers the character representation of the internal state of a test object
  subroutine test_base_get_as_char(this, repr)

    !> Instance
    class(test_base), intent(in) :: this

    !> Character representation, or **unallocated** on exit, if there is none
    character(:), allocatable, intent(out) :: repr

  end subroutine test_base_get_as_char

end module fortuno_basetypes
