! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains various types related to character representations.
module fortuno_chartypes
  use fortuno_utils, only : as_char, as_upper, nl, string
  implicit none

  private
  public :: char_rep
  public :: char_rep_int
  public :: named_item, named_details, named_state


  !> Character representable object.
  type, abstract :: char_rep
  contains
    procedure(char_rep_as_char), deferred :: as_char
  end type char_rep


  abstract interface

    !> Character representation of the char_rep object.
    function char_rep_as_char(this) result(repr)
      import :: char_rep
      implicit none

      !> Instance
      class(char_rep), intent(in) :: this

      !> Character representation of the object.
      character(:), allocatable :: repr

    end function char_rep_as_char

  end interface


  !> Implements a named item of arbitrary type
  type :: named_item

    !> Name
    character(:), allocatable :: name

    !> Value associated with the name
    class(*), allocatable :: value

  end type named_item


  ! Workaround:gfortran:13.2
  ! Needs user defined structure constructor as default constructor can not deal with class(*) field
  interface named_item
    module procedure new_named_item
  end interface


  !> Represents failure details with an array of named items.
  type, extends(char_rep) :: named_details

    !> Items containing the information about the failure details
    type(named_item), allocatable :: items(:)

  contains
    procedure :: as_char => named_details_as_char
  end type named_details


  !> Represents test internal state with an array of named items.
  type, extends(char_rep) :: named_state

    !> Items containing the information about the failure details
    type(named_item), allocatable :: items(:)

  contains
    procedure :: as_char => named_state_as_char
  end type named_state


  !> Character representable integer.
  type, extends(char_rep) :: char_rep_int

    !> Value
    integer :: value

  contains
    procedure :: as_char => char_rep_int_as_char
  end type char_rep_int

contains


  !> Returns the character representation of the failure details.
  function named_details_as_char(this) result(repr)

    !> Instance
    class(named_details), intent(in) :: this

    !> Character representation
    character(:), allocatable :: repr

    if (.not. allocated(this%items)) then
      repr = ""
      return
    end if
    call get_named_items_as_char_(this%items, repr, itemsep=nl, namesep=": ",&
        & capitalizename=.true.)

  end function named_details_as_char


  !> Returns the character representation of an internal test state.
  function named_state_as_char(this) result(repr)

    !> Instance
    class(named_state), intent(in) :: this

    !> Character representation
    character(:), allocatable :: repr

    if (.not. allocated(this%items)) then
      repr = ""
      return
    end if
    call get_named_items_as_char_(this%items, repr, itemsep=nl, namesep=":",&
        & capitalizename=.false.)

  end function named_state_as_char


  !> Integer with string representation.
  function char_rep_int_as_char(this) result(repr)

    !> Instance
    class(char_rep_int), intent(in) :: this

    !> Character representation
    character(:), allocatable :: repr

    repr = as_char(this%value)

  end function char_rep_int_as_char


  !> Explicit constructor for named_item (to avoid gfortran compilation problems)
  function new_named_item(name, val) result(this)

    !> Name of the item
    character(*), intent(in) :: name

    !> Value of the item
    class(*), intent(in) :: val

    !> Initialized instance
    type(named_item) :: this

    this%name = name
    allocate(this%value, source=val)

  end function new_named_item


  !! Returns the character representation of an array of named items.
  subroutine get_named_items_as_char_(items, repr, itemsep, namesep, capitalizename)
    type(named_item), intent(in) :: items(:)
    character(:), allocatable, intent(out) :: repr
    character(*), intent(in) :: itemsep, namesep
    logical, intent(in) :: capitalizename

    integer :: nitems, iitem, pos, reprlen, itemseplen, nameseplen
    type(string), allocatable :: valuestrings(:)

    nitems = size(items)
    if (nitems == 0) then
      repr = ""
      return
    end if
    reprlen = 0
    allocate(valuestrings(size(items)))
    do iitem = 1, nitems
      reprlen = reprlen + len(items(iitem)%name)
      select type (namedvalue => items(iitem)%value)
      type is (character(*))
        valuestrings(iitem)%content = namedvalue
      class is (string)
        valuestrings(iitem)%content = namedvalue%content
      class is (char_rep)
        valuestrings(iitem)%content = namedvalue%as_char()
      class default
        valuestrings(iitem)%content = "???"
      end select
      reprlen = reprlen + len(valuestrings(iitem)%content)
    end do

    nameseplen = len(namesep)
    itemseplen =  len(itemsep)
    reprlen = reprlen + nitems * nameseplen + (nitems - 1) * itemseplen
    allocate(character(reprlen) :: repr)

    pos = 1
    do iitem = 1, nitems
      associate(name => items(iitem)%name, valstr => valuestrings(iitem)%content)
        reprlen = len(name)
        repr(pos : pos + reprlen - 1) = name
        if (capitalizename) repr(pos:pos) = as_upper(repr(pos:pos))
        pos = pos + reprlen
        repr(pos : pos + nameseplen - 1) = namesep
        pos = pos + nameseplen
        reprlen = len(valstr)
        repr(pos : pos + reprlen - 1) = valstr
        pos = pos + reprlen
        if (iitem /= nitems) then
          repr(pos : pos + itemseplen - 1) = itemsep
          pos = pos + itemseplen
        end if
      end associate
    end do

  end subroutine get_named_items_as_char_

end module fortuno_chartypes
