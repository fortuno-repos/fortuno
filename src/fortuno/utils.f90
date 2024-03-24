! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Various helper utilities for the different modules
module fortuno_utils
  use iso_fortran_env, only : stderr => error_unit, stdout => output_unit
  implicit none

  private
  public :: ansicolors
  public :: as_char
  public :: basename
  public :: nl
  public :: stderr, stdout
  public :: string, string_list
  public :: as_upper

  !> New line character
  character(*), parameter :: nl = new_line("")

  !! Helper type for listing ansi terminal colors
  type :: ansi_colors_enum_
    character(4) :: default = char(27) // "[0m"
    character(5) :: red = char(27) // "[31m"
    character(5) :: green = char(27) // "[32m"
    character(5) :: yellow = char(27) // "[33m"
    character(5) :: blue = char(27) // "[34m"
    character(5) :: magenta = char(27) // "[35m"
    character(5) :: cyan = char(27) // "[36m"
    character(5) :: white = char(27) // "[37m"
  end type ansi_colors_enum_

  !> Contains a list of ansi colors to use in Fortuno
  !!
  !! Supports currently (default, red, green, yellow, magenta, cyan and white)
  !!
  type(ansi_colors_enum_), parameter :: ansicolors = ansi_colors_enum_()


  interface as_char
    module procedure integer_as_char
  end interface as_char


  !> Minimalistic string type
  type :: string

    !> Actual content of the string
    character(:), allocatable :: content

  end type string


  !> Minimalistic string list type
  type :: string_list

    !> Actual items in the list
    type(string), allocatable :: items(:)

  end type string_list

contains

  !> Returns the character representation of an integer value
  pure function integer_as_char(val) result(repr)

    !> Integer value to represent
    integer, intent(in) :: val

    !> Character representation
    character(:), allocatable :: repr

    ! should be enough to represent up to 128 bit integers with sign
    character(40) :: buffer

    write(buffer, "(i0)") val
    repr = trim(buffer)

  end function integer_as_char


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
  pure function as_upper(str) result(upperstr)

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

  end function as_upper

end module fortuno_utils