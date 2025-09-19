! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Global serial context to avoid explicit passing of context when using non-threaded serial driver
module fortuno_serial_serialglobalctx
  use fortuno, only : check_result, stringable, test_ptr_item
  use fortuno_serial_serialcontext, only : serial_context
  implicit none

  private
  public :: serialglobalctx
  public :: set_serial_global_context
  public :: serial_check, serial_check_failed, serial_failed
  public :: serial_skip, serial_skipped
  public :: serial_store_state
  public :: serial_scope_pointers


  !> Generic check interface accessing the global context
  interface serial_check
    module procedure serial_check_logical, serial_check_check_result
  end interface serial_check


  !> The global context
  type(serial_context), pointer, protected :: serialglobalctx => null()

contains

  !> Sets the global context
  subroutine set_serial_global_context(newctx, oldctx)

    !> New context to use as global context
    type(serial_context), pointer, intent(in) :: newctx

    !> Old context used so far as global context
    type(serial_context), pointer, optional, intent(out) :: oldctx

    if (present(oldctx)) oldctx => serialglobalctx
    serialglobalctx => newctx

  end subroutine set_serial_global_context


  !> Check using global context logical input
  subroutine serial_check_logical(cond, msg, file, line)

    !> Whether check condition is fulfilled (check is successful)
    logical, intent(in) :: cond

    !> Check message
    character(*), optional, intent(in) :: msg

    !> Source file name
    character(*), optional, intent(in) :: file

    !> Line information
    integer, optional, intent(in) :: line

    call serialglobalctx%check(cond, msg=msg, file=file, line=line)

  end subroutine serial_check_logical


  !> Check using global context check_result input
  subroutine serial_check_check_result(checkresult, msg, file, line)

    !> Whether check condition is fulfilled (check is successful)
    type(check_result), intent(in) :: checkresult

    !> Check message
    character(*), optional, intent(in) :: msg

    !> Source file name
    character(*), optional, intent(in) :: file

    !> Line information
    integer, optional, intent(in) :: line

    call serialglobalctx%check(checkresult, msg=msg, file=file, line=line)

  end subroutine serial_check_check_result


  !> Whether the last check has failed
  function serial_check_failed() result(check_failed)

    !> True, if last check has failed
    logical :: check_failed

    check_failed = serialglobalctx%check_failed()

  end function serial_check_failed


  !> Whether the test/context has failed
  function serial_failed() result(failed)

    !> True, if test has failed already (e.g. due to previous failing checks)
    logical :: failed

    failed = serialglobalctx%failed()

  end function serial_failed


  !> Mark current test/context as skipped
  subroutine serial_skip()

    call serialglobalctx%skip()

  end subroutine serial_skip


  !> Whether test had been marked as skipped
  function serial_skipped() result(skipped)

    !> Skip status
    logical :: skipped

    skipped = serialglobalctx%skipped()

  end function serial_skipped


  !> Returns the enclosing suite pointers
  function serial_scope_pointers() result(scopeptrs)

    !> Pointers to enclosing suites
    type(test_ptr_item), allocatable :: scopeptrs(:)

    scopeptrs = serialglobalctx%scope_pointers()

  end function serial_scope_pointers


  !> Stores the test state for later introspection
  subroutine serial_store_state(state)

    !> State to store
    class(stringable), intent(in) :: state

    call serialglobalctx%store_state(state)

  end subroutine serial_store_state

end module fortuno_serial_serialglobalctx
