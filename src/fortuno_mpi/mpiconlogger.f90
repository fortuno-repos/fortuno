! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains the implementation of the test logger for the mpi driver
module fortuno_mpi_mpiconlogger
  use fortuno, only : as_char, console_logger, failure_info, nl
  use fortuno_mpi_mpienv, only : mpi_env
  use fortuno_mpi_mpitestinfo, only : mpi_failure_location
  implicit none

  private
  public :: init_mpi_console_logger, mpi_console_logger


  !> Implements an mpi logger which logs to a formatted unit
  type, extends(console_logger) :: mpi_console_logger
    type(mpi_env) :: mpienv
  contains
    procedure :: is_active => mpi_console_logger_is_active
    procedure :: start_drive => mpi_console_logger_start_drive
    procedure :: get_failure_info_repr => mpi_console_logger_get_failure_info_repr
  end type mpi_console_logger

contains

  !> Initializes an mpi_console_logger instance
  subroutine init_mpi_console_logger(this, mpienv)

    !> Instance
    type(mpi_console_logger), intent(out) :: this

    !> MPI environment
    type(mpi_env), intent(in) :: mpienv

    this%mpienv = mpienv

  end subroutine init_mpi_console_logger


  !> Returns whether the logger is active
  function mpi_console_logger_is_active(this) result(isactive)

    !> Instance
    class(mpi_console_logger), intent(in) :: this

    !> Whether logger is active
    logical :: isactive

    isactive = this%mpienv%rank == 0

  end function mpi_console_logger_is_active


  !> Starts the logging, called before any other procedures are called
  subroutine mpi_console_logger_start_drive(this)

    !> Instance
    class(mpi_console_logger), intent(inout) :: this

    if (.not. this%is_active()) return
    call this%console_logger%start_drive()
    call this%log_message(nl // "Nr. of ranks: " // as_char(this%mpienv%nranks))

  end subroutine mpi_console_logger_start_drive


  !> Returns the representation of the failure information (called collectively)
  subroutine mpi_console_logger_get_failure_info_repr(this, failureinfo, location, message,&
      & details)

    !> Instance
    class(mpi_console_logger), intent(in) :: this

    !> Failure info to get the representation from
    type(failure_info), allocatable, intent(in) :: failureinfo

    !> Location string (unallocated if not available or not relevant)
    character(:), allocatable, intent(out) :: location

    !> Message string (unallocated if not available or not relevant)
    character(:), allocatable, intent(out) :: message

    !> Details string (unallocated if not available or not relevant)
    character(:), allocatable, intent(out) :: details

    character(:), allocatable :: buffer
    integer, allocatable :: failedranks(:)

    select type (loc => failureinfo%location)
    class is (mpi_failure_location)
      failedranks = loc%failedranks
    class default
      error stop "invalid failureinfo%location type in mpi_console_logger_get_failure_info_repr"
    end select

    if (failedranks(1) == 0) then
      if (this%mpienv%rank == 0) then
        buffer = failureinfo%location%as_char()
        if (len(buffer) > 0) call move_alloc(buffer, location)
        if (allocated(failureinfo%message)) message = failureinfo%message
        if (allocated(failureinfo%details)) details = failureinfo%details%as_char()
      end if
    else if (this%mpienv%rank == 0) then
      call this%mpienv%recv_alloc_char(location, failedranks(1))
      call this%mpienv%recv_alloc_char(message, failedranks(1))
      call this%mpienv%recv_alloc_char(details, failedranks(1))
    else if (this%mpienv%rank == failedranks(1)) then
      buffer = failureinfo%location%as_char()
      if (len(buffer) == 0) deallocate(buffer)
      call this%mpienv%send_alloc_char(buffer, 0)
      call this%mpienv%send_alloc_char(failureinfo%message, 0)
      if (allocated(buffer)) deallocate(buffer)
      if (allocated(failureinfo%details)) buffer = failureinfo%details%as_char()
      call this%mpienv%send_alloc_char(buffer, 0)
    end if

  end subroutine mpi_console_logger_get_failure_info_repr

end module fortuno_mpi_mpiconlogger