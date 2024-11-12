! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

module test_simple
  use mylib, only : broadcast
  use fortuno_coarray, only : test => coa_pure_case_item, context => coa_context, is_equal, str,&
      & test_list
  implicit none

contains


  !> Returns the tests from this module
  function tests()
    type(test_list) :: tests

    tests = test_list([&
        test("broadcast", test_broadcast)&
    ])

  end function tests


  !> Broadcast test with collective communication.
  !!
  !! Note: as coarray parallelism might be implemented via threads, coarray tests must be "pure" and
  !! use the passed context object to signalize test events.
  !!
  subroutine test_broadcast(ctx)
    class(context), intent(inout) :: ctx

    integer, parameter :: sourceimg = 1, sourceval = 100, otherval = -1
    integer :: buffer

    character(:), allocatable :: msg

    ! GIVEN source rank contains a different integer value as all other ranks
    if (this_image() == sourceimg) then
      buffer = sourceval
    else
      buffer = otherval
    end if

    ! WHEN source rank broadcasts its value
    call broadcast(buffer, sourceimg)

    ! Make every third rank fail for demonstration purposes
    if (mod(this_image() - 1, 3) == 2) then
      buffer = sourceval + 1
      msg = "Failing on image " //  str(this_image()) // " on purpose"
    end if

    ! THEN each rank must contain source rank's value
    !
    ! Note: check() calls are collective calls, all images must call it with their local result
    ! synchronously.
    !
    call ctx%check(is_equal(buffer, sourceval), msg=msg)

  end subroutine test_broadcast

end module test_simple
