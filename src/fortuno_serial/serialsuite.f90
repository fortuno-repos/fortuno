! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains a trivial implementation for a serial suite
module fortuno_serial_serialsuite
  use fortuno, only : test_item, test_list
  use fortuno_serial_serialbasetypes, only : serial_suite_base
  implicit none

  private
  public :: serial_suite, serial_suite_item


  !> Base type for all serial suites
  type, extends(serial_suite_base) :: serial_suite
  end type serial_suite

contains

  !> Returns a serial suite instance wrapped as test_item
  function serial_suite_item(name, tests) result(testitem)
    character(*), intent(in) :: name
    type(test_list), intent(in) :: tests
    type(test_item) :: testitem

    call testitem%init(serial_suite(name=name, tests=tests))

  end function serial_suite_item

end module fortuno_serial_serialsuite
