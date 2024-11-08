! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains various types related to character representations.
module fortuno_chartypes
  use fortuno_env, only : i32, i64, r32, r64, nl
  use fortuno_checkfuncs, only : is_close_elem
  use fortuno_utils, only : str, upper, string_item
  implicit none

  private
  public :: stringable
  public :: int32_item, int64_item
  public :: int32_r1_item, int64_r1_item
  public :: real32_item, real64_item
  public :: dict_item, details_dict, state_dict
  public :: matches_type_value
  public :: get_ptr_to


  !> Interface of a character representable object.
  type, abstract :: stringable
  contains
    procedure(stringable_as_string), deferred :: as_string
  end type stringable


  abstract interface

    !> Character representation of the stringable object.
    function stringable_as_string(this) result(repr)
      import :: stringable
      implicit none

      !> Instance
      class(stringable), intent(in) :: this

      !> Character representation of the object.
      character(:), allocatable :: repr

    end function stringable_as_string

  end interface


  !> Implements a named item of arbitrary type
  type :: dict_item

    !> Name
    character(:), allocatable :: name

    !> Value associated with the name
    class(*), allocatable :: value

  end type dict_item


  ! Workaround:gfortran:13.2
  ! Needs user defined structure constructor as default constructor can not deal with class(*) field
  interface dict_item
    module procedure new_dict_item
  end interface


  !> Represents failure details with an array of named items.
  type, extends(stringable) :: details_dict

    !> Items containing the information about the failure details
    type(dict_item), allocatable :: items(:)

  contains
    procedure :: as_string => details_dict_as_string
  end type details_dict


  !> Represents test internal state with an array of named items.
  type, extends(stringable) :: state_dict

    !> Items containing the information about the failure details
    type(dict_item), allocatable :: items(:)

  contains
    procedure :: as_string => state_dict_as_string
  end type state_dict


  !> Character representable 32 bit integer.
  type, extends(stringable) :: int32_item

    !> Value
  integer(i32), allocatable :: value

  contains
    procedure :: as_string => int32_item_as_string
  end type int32_item


  !> Character representable 32 bit integer rank 1 array.
  type, extends(stringable) :: int32_r1_item

    !> Value
  integer(i32), allocatable :: value(:)

  contains
    procedure :: as_string => int32_r1_item_as_string
  end type int32_r1_item


  !> Character representable 64 bit integer.
  type, extends(stringable) :: int64_item

    !> Value
  integer(i64), allocatable :: value

  contains
    procedure :: as_string => int64_item_as_string
  end type int64_item


  !> Character representable 64 bit integer rank 1 array.
  type, extends(stringable) :: int64_r1_item

    !> Value
  integer(i64), allocatable :: value(:)

  contains
    procedure :: as_string => int64_r1_item_as_string
  end type int64_r1_item


  !> Character representable 32 bit float.
  type, extends(stringable) :: real32_item

    !> Value
    real(r32), allocatable :: value

  contains
    procedure :: as_string => real32_item_as_string

  end type real32_item


  !> Character representable 64 bit float.
  type, extends(stringable) :: real64_item

    !> Value
    real(r64), allocatable :: value

  contains
    procedure :: as_string => real64_item_as_string

  end type real64_item


  interface get_ptr_to
    module procedure get_ptr_to_details_dict
    module procedure get_ptr_to_state_dict
  end interface get_ptr_to


  interface matches_type_value
    module procedure matches_type_value_int32_item
    module procedure matches_type_value_int32_r1_item
    module procedure matches_type_value_int64_item
    module procedure matches_type_value_int64_r1_item
    module procedure matches_type_value_real32_item
    module procedure matches_type_value_real64_item
    module procedure matches_type_value_string
  end interface matches_type_value

contains


  !> Returns the character representation of the failure details.
  function details_dict_as_string(this) result(repr)

    !> Instance
    class(details_dict), intent(in) :: this

    !> Character representation
    character(:), allocatable :: repr

    if (.not. allocated(this%items)) then
      repr = ""
      return
    end if
    call get_dict_items_as_string(this%items, repr, itemsep=nl, namesep=": ",&
        & capitalizenames=.true.)

  end function details_dict_as_string


  !> Returns the character representation of an internal test state.
  function state_dict_as_string(this) result(repr)

    !> Instance
    class(state_dict), intent(in) :: this

    !> Character representation
    character(:), allocatable :: repr

    if (.not. allocated(this%items)) then
      repr = ""
      return
    end if
    call get_dict_items_as_string(this%items, repr, itemsep=", ", namesep=": ",&
        & capitalizenames=.false.)

  end function state_dict_as_string


  !> String representation of an integer rank 1 array.
  function int32_item_as_string(this) result(repr)

    !> Instance
    class(int32_item), intent(in) :: this

    !> Character representation
    character(:), allocatable :: repr

    repr = str(this%value)

  end function int32_item_as_string


  !> String representation of an integer rank 1 array.
  function int32_r1_item_as_string(this) result(repr)

    !> Instance
    class(int32_r1_item), intent(in) :: this

    !> Character representation
    character(:), allocatable :: repr

    repr = str(this%value)

  end function int32_r1_item_as_string


  !> String representation of an integer rank 1 array.
  function int64_item_as_string(this) result(repr)

    !> Instance
    class(int64_item), intent(in) :: this

    !> Character representation
    character(:), allocatable :: repr

    repr = str(this%value)

  end function int64_item_as_string


  !> String representation of an integer rank 1 array.
  function int64_r1_item_as_string(this) result(repr)

    !> Instance
    class(int64_r1_item), intent(in) :: this

    !> Character representation
    character(:), allocatable :: repr

    repr = str(this%value)

  end function int64_r1_item_as_string


  !> String representation of a 32 bit real.
  function real32_item_as_string(this) result(repr)

    !> Instance
    class(real32_item), intent(in) :: this

    !> Character representation
    character(:), allocatable :: repr

    repr = str(this%value)

  end function real32_item_as_string


  !> String representation of a 32 bit real.
  function real64_item_as_string(this) result(repr)

    !> Instance
    class(real64_item), intent(in) :: this

    !> Character representation
    character(:), allocatable :: repr

    repr = str(this%value)

  end function real64_item_as_string


  !> Explicit constructor for dict_item (to avoid gfortran compilation problems)
  function new_dict_item(name, val) result(this)

    !> Name of the item
    character(*), intent(in) :: name

    !> Value of the item
    class(*), intent(in) :: val

    !> Initialized instance
    type(dict_item) :: this

    this%name = name
    allocate(this%value, source=val)

  end function new_dict_item


  subroutine get_ptr_to_details_dict(obj, ptr)
    class(stringable), pointer, intent(in) :: obj
    type(details_dict), pointer, intent(out) :: ptr

    ptr => null()
    select type (obj)
    type is (details_dict)
      ptr => obj
    end select

  end subroutine get_ptr_to_details_dict


  subroutine get_ptr_to_state_dict(obj, ptr)
    class(stringable), pointer, intent(in) :: obj
    type(state_dict), pointer, intent(out) :: ptr

    ptr => null()
    select type (obj)
    type is (state_dict)
      ptr => obj
    end select

  end subroutine get_ptr_to_state_dict


  function matches_type_value_int32_item(obj, val) result(matches)
    class(*), intent(in) :: obj
    integer(i32), intent(in) :: val
    logical :: matches

    matches = .false.
    select type (obj)
    type is (int32_item)
      matches = obj%value == val
    end select

  end function matches_type_value_int32_item


  function matches_type_value_int32_r1_item(obj, val) result(matches)
    class(*), intent(in) :: obj
    integer(i32), intent(in) :: val(:)
    logical :: matches

    matches = .false.
    select type (obj)
    type is (int32_r1_item)
      if (.not. all(shape(obj%value, kind=i64) == shape(val, kind=i64))) return
      matches = all(obj%value == val)
    end select

  end function matches_type_value_int32_r1_item


  function matches_type_value_int64_item(obj, val) result(matches)
    class(*), intent(in) :: obj
    integer(i64), intent(in) :: val
    logical :: matches

    matches = .false.
    select type (obj)
    type is (int64_item)
      matches = obj%value == val
    end select

  end function matches_type_value_int64_item


  function matches_type_value_int64_r1_item(obj, val) result(matches)
    class(*), intent(in) :: obj
    integer(i64), intent(in) :: val(:)
    logical :: matches

    matches = .false.
    select type (obj)
    type is (int64_r1_item)
      if (.not. all(shape(obj%value, kind=i64) == shape(val, kind=i64))) return
      matches = all(obj%value == val)
    end select

  end function matches_type_value_int64_r1_item


  function matches_type_value_real32_item(obj, val, rtol, atol) result(matches)
    class(*), intent(in) :: obj
    real(r32), intent(in) :: val
    real(r32), optional, intent(in) :: rtol, atol
    logical :: matches

    matches = .false.
    select type (obj)
    type is (real32_item)
      matches = is_close_elem(obj%value, val, rtol, atol)
    end select

  end function matches_type_value_real32_item


  function matches_type_value_real64_item(obj, val, rtol, atol) result(matches)
    class(*), intent(in) :: obj
    real(r64), intent(in) :: val
    real(r64), optional, intent(in) :: rtol, atol
    logical :: matches

    matches = .false.
    select type (obj)
    type is (real64_item)
      matches = is_close_elem(obj%value, val, rtol, atol)
    end select

  end function matches_type_value_real64_item


  function matches_type_value_string(obj, val) result(matches)
    class(*), intent(in) :: obj
    type(character(*)), intent(in) :: val
    logical :: matches

    matches = .false.
    select type (obj)
    type is (character(*))
      matches = obj == val
    end select

  end function matches_type_value_string


  !! Returns the character representation of an array of named items.
  subroutine get_dict_items_as_string(items, repr, itemsep, namesep, capitalizenames)
    type(dict_item), intent(in) :: items(:)
    character(:), allocatable, intent(out) :: repr
    character(*), intent(in) :: itemsep, namesep
    logical, intent(in) :: capitalizenames

    integer :: nitems, iitem, pos, reprlen, itemseplen, nameseplen
    type(string_item), allocatable :: valuestrings(:)

    nitems = size(items)
    if (nitems == 0) then
      repr = ""
      return
    end if
    reprlen = 0
    allocate(valuestrings(size(items)))
    do iitem = 1, nitems
      reprlen = reprlen + len(items(iitem)%name)
      select type (itemvalue => items(iitem)%value)
      type is (character(*))
        valuestrings(iitem)%value = itemvalue
      type is (logical)
        valuestrings(iitem)%value = str(itemvalue)
      type is (integer(i32))
        valuestrings(iitem)%value = str(itemvalue)
      type is (integer(i64))
        valuestrings(iitem)%value = str(itemvalue)
      type is (real(r32))
        valuestrings(iitem)%value = str(itemvalue)
      type is (real(r64))
        valuestrings(iitem)%value = str(itemvalue)
      class is (stringable)
        valuestrings(iitem)%value = itemvalue%as_string()
      class default
        valuestrings(iitem)%value = "???"
      end select
      reprlen = reprlen + len(valuestrings(iitem)%value)
    end do

    nameseplen = len(namesep)
    itemseplen =  len(itemsep)
    reprlen = reprlen + nitems * nameseplen + (nitems - 1) * itemseplen
    allocate(character(reprlen) :: repr)

    pos = 1
    do iitem = 1, nitems
      associate(name => items(iitem)%name, valstr => valuestrings(iitem)%value)
        reprlen = len(name)
        repr(pos : pos + reprlen - 1) = name
        if (capitalizenames) repr(pos:pos) = upper(repr(pos:pos))
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

  end subroutine get_dict_items_as_string

end module fortuno_chartypes
