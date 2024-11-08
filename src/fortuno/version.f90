! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains version information
module fortuno_version
  use fortuno_utils, only : str
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

    versionstr = str(versions(1)) // "." // str(versions(2))
    if (versions(3) /= 0) versionstr = versionstr // "." // str(versions(3))

  end function version_string

end module fortuno_version
