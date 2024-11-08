! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Various helper utilities for the different modules
module fortuno_utils
  use fortuno_env, only : r32, r64, i32, i64
  implicit none

  private
  public :: str
  public :: basename
  public :: upper
  public :: string_item, string_item_list


  interface str
    module procedure str_int32
    module procedure str_int32_r1
    module procedure str_int64
    module procedure str_int64_r1
    module procedure str_real32
    module procedure str_real64
    module procedure str_logical
  end interface str


  !> Minimalistic string type
  type :: string_item

    !> Actual content of the string
    character(:), allocatable :: value

  end type string_item


  !> Minimalistic string list type
  type :: string_item_list

    !> Actual items in the list
    type(string_item), allocatable :: items(:)

  end type string_item_list

contains


  !> Returns the character representation of an integer value
  pure function str_int32(val) result(repr)

    !> Integer value to represent
    integer(i32), intent(in) :: val

    !> Character representation
    character(:), allocatable :: repr

    character(11) :: buffer

    write(buffer, "(i11)") val
    repr = trim(adjustl(buffer))

  end function str_int32


  !> Returns the character representation of an integer rank 1 array.
  pure function str_int32_r1(val) result(repr)

    !> Integer array to represent
    integer(i32), intent(in) :: val(:)

    !> Character representation
    character(:), allocatable :: repr

    character(*), parameter :: separator = ", "
    type(string_item) :: reps(size(val))
    integer :: replens(size(val))
    integer :: ii, nn, replen, pos

    if (size(val) == 0) then
      repr = "[]"
      return
    end if

    nn = size(val)
    do ii = 1, nn
      reps(ii)%value = str(val(ii))
      replens(ii) = len(reps(ii)%value)
    end do

    ! take delimiting braces and ", " separator into account
    replen = sum(replens) + 2 + (nn - 1) * len(separator)
    allocate(character(replen) :: repr)
    repr(1:1) = "["
    pos = 2
    do ii = 1, nn
      repr(pos : pos + replens(ii) - 1) = reps(ii)%value
      pos = pos + replens(ii)
      if (ii /= nn) then
        repr(pos : pos + len(separator) - 1) = separator
        pos = pos + len(separator)
      end if
    end do
    repr(pos:pos) = "]"

  end function str_int32_r1


  !> Returns the character representation of an integer value
  pure function str_int64(val) result(repr)

    !> Integer value to represent
    integer(i64), intent(in) :: val

    !> Character representation
    character(:), allocatable :: repr

    character(20) :: buffer

    write(buffer, "(i20)") val
    repr = trim(adjustl(buffer))

  end function str_int64


  !> Returns the character representation of an integer rank 1 array.
  pure function str_int64_r1(val) result(repr)

    !> Integer array to represent
    integer(i64), intent(in) :: val(:)

    !> Character representation
    character(:), allocatable :: repr

    character(*), parameter :: separator = ", "
    type(string_item) :: reps(size(val))
    integer :: replens(size(val))
    integer :: ii, nn, replen, pos

    if (size(val) == 0) then
      repr = "[]"
      return
    end if

    nn = size(val)
    do ii = 1, nn
      reps(ii)%value = str(val(ii))
      replens(ii) = len(reps(ii)%value)
    end do

    ! take delimiting braces and ", " separator into account
    replen = sum(replens) + 2 + (nn - 1) * len(separator)
    allocate(character(replen) :: repr)
    repr(1:1) = "["
    pos = 2
    do ii = 1, nn
      repr(pos : pos + replens(ii) - 1) = reps(ii)%value
      pos = pos + replens(ii)
      if (ii /= nn) then
        repr(pos : pos + len(separator) - 1) = separator
        pos = pos + len(separator)
      end if
    end do
    repr(pos:pos) = "]"

  end function str_int64_r1


  !> Returns the character representation of an integer value
  pure function str_real32(val) result(repr)

    !> Integer value to represent
    real(r32), intent(in) :: val

    !> Character representation
    character(:), allocatable :: repr

    character(16) :: buffer

    write(buffer, "(g16.7)") val
    repr = trim(adjustl(buffer))

  end function str_real32


  !> Returns the character representation of an integer value
  pure function str_real64(val) result(repr)

    !> Integer value to represent
    real(r64), intent(in) :: val

    !> Character representation
    character(:), allocatable :: repr

    character(26) :: buffer

    write(buffer, "(g26.16)") val
    repr = trim(buffer)

  end function str_real64


  !> Returns the character representation of an integer value
  pure function str_logical(val) result(repr)

    !> Integer value to represent
    logical, intent(in) :: val

    !> Character representation
    character(:), allocatable :: repr

    if (val) then
      repr = "T"
    else
      repr = "F"
    end if

  end function str_logical


  !> Returns the last component (base name) of a slash ("/") separated path
  pure function basename(path)

    !> Path to split
    character(*), intent(in) :: path

    !> The basename
    character(:), allocatable :: basename

    integer :: lastseppos

    lastseppos = index(path, "/", back=.true.)
    basename = path(lastseppos + 1 :)

  end function basename


  !> Converts a string to upper-case.
  pure function upper(str) result(upperstr)

    !> String to convert
    character(*), intent(in) :: str

    !> Upper-case string
    character(len(str)) :: upperstr

    integer, parameter :: lowerstart = iachar("a")
    integer, parameter :: lowerend = iachar("z")
    integer, parameter :: shift = iachar("A") - lowerstart

    integer :: ii, ord

    do ii = 1, len(str)
      ord = iachar(str(ii:ii))
      if (ord >= lowerstart .and. ord <= lowerend) then
        upperstr(ii:ii) = achar(iachar(str(ii:ii)) + shift)
      else
        upperstr(ii:ii) = str(ii:ii)
      end if
    end do

  end function upper

end module fortuno_utils