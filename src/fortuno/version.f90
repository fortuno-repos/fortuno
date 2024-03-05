! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains version information
module fortuno_version
  use fortuno_utils, only : as_char
  implicit none

  private
  public :: versions
  public :: version_string

  !> Major, minor and patch version numbers
  integer, parameter :: versions(3) = [0, 1, 0]

contains

  !> Returns the character representation of the current version
  function version_string() result(versionstr)

    !> Character representation of the version
    character(:), allocatable :: versionstr

    versionstr = as_char(versions(1)) // "." // as_char(versions(2))
    if (versions(3) /= 0) versionstr = versionstr // "." // as_char(versions(3))

  end function version_string

end module fortuno_version
