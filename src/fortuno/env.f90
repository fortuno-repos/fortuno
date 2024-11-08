! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Various helper utilities for the different modules
module fortuno_env
  use iso_fortran_env, only : stderr => error_unit, stdout => output_unit, i32 => int32,&
      & i64 => int64, r32 => real32, r64 => real64
  implicit none

  private
  public :: stderr, stdout
  public :: r32, r64, i32, i64
  public :: ansicolors
  public :: nl


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


  !> New line character
  character(*), parameter :: nl = new_line("")

end module fortuno_env
