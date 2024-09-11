! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains a type to deal with the mpi environment
module fortuno_mpi_mpienv
  use mpi_f08, only : MPI_Allreduce, MPI_CHAR, MPI_Comm, MPI_Comm_rank, MPI_Comm_size,&
      & MPI_COMM_WORLD, MPI_IN_PLACE, MPI_Init, MPI_INTEGER, MPI_Finalize, MPI_PROD, MPI_Recv,&
      & MPI_Send, MPI_Status
  implicit none

  private
  public :: init_mpi_env, final_mpi_env, mpi_env


  !> Contains main charateristics of the MPI environment
  type :: mpi_env

    !> MPI communicator
    type(MPI_Comm) :: comm

    !> Nr. of ranks in the communicator
    integer :: nranks

    !> Rank of the current process
    integer :: rank

  contains
    procedure :: reduce_and => mpi_env_reduce_and
    procedure :: send_alloc_char => mpi_env_send_alloc_char
    procedure :: recv_alloc_char => mpi_env_recv_alloc_char
  end type mpi_env

contains

  !> Initializes the MPI environment
  subroutine init_mpi_env(this)

    !> Instance
    type(mpi_env), intent(out) :: this

    integer :: ierror

    call MPI_Init(ierror)
    if (ierror /= 0) error stop "MPI_Init failed in init_mpi_env"
    this%comm = MPI_COMM_WORLD
    call MPI_Comm_size(this%comm, this%nranks, ierror)
    if (ierror /= 0) error stop "MPI_Comm_size failed in init_mpi_env"
    call MPI_Comm_rank(this%comm, this%rank, ierror)
    if (ierror /= 0) error stop "MPI_Comm_rank failed in init_mpi_env"

  end subroutine init_mpi_env


  !> Finalizes the MPI environment
  subroutine final_mpi_env(this)

    !> Instance
    type(mpi_env), intent(out) :: this

    integer :: ierror

    call MPI_Finalize(ierror)
    if (ierror /= 0) error stop "MPI_Finalize failed in execute_mpi_cmd_app"

  end subroutine final_mpi_env


  !> Reduces a logical array using the and operator
  subroutine mpi_env_reduce_and(this, array)

    !> Instance
    class(mpi_env), intent(in) :: this

    !> Array to reduce with the and operator
    logical, intent(inout) :: array(:)

    integer :: tmpint(size(array))
    integer :: ierror

    ! workaround: ifort and intelmpi
    ! allreduce() with MPI_LOGICALS and MPI_LAND seems to result in some broken logical
    ! representation resulting in incorrect findloc(..., dim=1) results. Therefore, map logicals
    ! to integers before reduction and map them back afterwards again.
    tmpint(:) = merge(1, 0, array)
    call MPI_Allreduce(MPI_IN_PLACE, tmpint, size(tmpint), MPI_INTEGER, MPI_PROD, this%comm, ierror)
    if (ierror /= 0) error stop "MPI_Allreduce failed in mpi_env_reduce_and"
    array(:) = tmpint == 1

  end subroutine mpi_env_reduce_and


  !> Sends an allocatable character to a process
  subroutine mpi_env_send_alloc_char(this, buffer, dest)

    !> Instance
    class(mpi_env), intent(in) :: this

    !> Buffer to send (can be unallocated)
    character(:), allocatable, intent(in) :: buffer

    !> Destination rank
    integer, intent(in) :: dest

    integer :: charlen, ierror

    if (allocated(buffer)) then
      charlen = len(buffer)
    else
      charlen = -1
    end if
    call MPI_Send(charlen, 1, MPI_INTEGER, dest, 1, this%comm, ierror)
    if (ierror /= 0) error stop "MPI_Send(1) failed in mpi_env_send_alloc_char"
    if (charlen > 0) then
      call MPI_Send(buffer, charlen, MPI_CHAR, dest, 2, this%comm, ierror)
      if (ierror /= 0) error stop "MPI_Send(2) failed in mpi_env_send_alloc_char"
    end if

  end subroutine mpi_env_send_alloc_char


  !> Receives an allocatable character from a process
  subroutine mpi_env_recv_alloc_char(this, buffer, src)

    !> Instance
    class(mpi_env), intent(in) :: this

    !> Received string (might be unallocated if it was unallocated on the sender side)
    character(:), allocatable, intent(out) :: buffer

    !> Source rank
    integer, intent(in) :: src

    integer :: charlen, ierror
    type(MPI_Status) :: mpistat

    call MPI_Recv(charlen, 1, MPI_INTEGER, src, 1, this%comm, mpistat, ierror)
    if (ierror /= 0) error stop "MPI_Recv(1) failed in mpi_env_recv_alloc_char"
    if (charlen >= 0) allocate(character(charlen) :: buffer)
    if (charlen > 0) then
      call MPI_Recv(buffer, charlen, MPI_CHAR, src, 2, this%comm, mpistat, ierror)
      if (ierror /= 0) error stop "MPI_Recv(2) failed in mpi_env_recv_alloc_char"
    end if

  end subroutine mpi_env_recv_alloc_char

end module fortuno_mpi_mpienv
