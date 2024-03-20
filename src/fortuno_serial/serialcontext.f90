! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Serial driver specific context
module fortuno_serial_serialcontext
  use fortuno, only : context_factory, test_context
  implicit none

  private
  public :: as_serial_context, serial_context
  public :: serial_context_factory


  !> Context used by tests driven through the serial driver
  type, extends(test_context) :: serial_context
  end type serial_context


  !> Factory to create serial context instances
  type, extends(context_factory) :: serial_context_factory
  contains
    procedure :: create_context => serial_context_factory_create_context
  end type serial_context_factory

contains

  !> Returns a serial_context class pointer to a generic context pointer
  function as_serial_context(trg) result(ptr)

    !> Target to point at
    class(test_context), pointer, intent(in) :: trg

    !> Class specific pointer
    type(serial_context), pointer :: ptr

    select type (trg)
    type is (serial_context)
      ptr => trg
    class default
      error stop "Invalid test context type obtained in as_serial_context"
    end select

  end function as_serial_context


  !> Creates a serial context instance
  subroutine serial_context_factory_create_context(this, ctx)

    !> Instance
    class(serial_context_factory), intent(inout) :: this

    !> Created context on exit
    class(test_context), allocatable, intent(out) :: ctx

    allocate(serial_context :: ctx)

  end subroutine serial_context_factory_create_context

end module fortuno_serial_serialcontext