! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains some built-in checkers
module fortuno_checkers
  use fortuno_checkers_int32, only : is_equal, all_equal
  use fortuno_checkers_int64, only : is_equal, all_equal
  use fortuno_checkers_real32, only : is_close, all_close
  use fortuno_checkers_real64, only : is_close, all_close
  implicit none

  private
  public :: is_equal, all_equal
  public :: is_close, all_close

end module fortuno_checkers