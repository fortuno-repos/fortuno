! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Demo module/library to be tested
module mylib
  implicit none

  private
  public :: broadcast

contains


  !> Broadcasts a scalar integer.
  subroutine broadcast(buffer, source)

    !> Buffer to broadcast
    integer, intent(inout) :: buffer

    !> Source image
    integer, intent(in) :: source

    integer :: stat

    call co_broadcast(buffer, source)

  end subroutine broadcast

end module mylib
