! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Contains a type to deal with the coarray environment
module fortuno_coarray_coaenv
  implicit none

  private
  public :: init_coa_env, final_coa_env, coa_env


  !> Contains main charateristics of the coarray environment
  type :: coa_env

    !> Nr. of images
    integer :: nimages

    !> Current image
    integer :: image

  contains
    procedure :: reduce_and => coa_env_reduce_and
    procedure :: broadcast_alloc_char => coa_env_broadcast_alloc_char
  end type coa_env

contains

  !> Initializes the coa environment
  subroutine init_coa_env(this)

    !> Instance
    type(coa_env), intent(out) :: this

    this%nimages = num_images()
    this%image = this_image()

  end subroutine init_coa_env


  !> Finalizes the coa environment
  subroutine final_coa_env(this)

    !> Instance
    type(coa_env), intent(out) :: this

  end subroutine final_coa_env


  !> Reduces a logical array using the and operator
  subroutine coa_env_reduce_and(this, array)

    !> Instance
    class(coa_env), intent(in) :: this

    !> Array to reduce with the and operator
    logical, intent(inout) :: array(:)

    ! call co_reduce(array, logical_and)
    ! Workaround:nagfor:7.2 (build 7202)
    block
      logical, allocatable :: mycond[:]
      integer :: ii
      allocate(mycond[*])
      mycond = array(this_image())
      sync all
      if (this_image() == 1) then
        do ii = 2, num_images()
          array(ii) = mycond[ii]
        end do
      end if
      call co_broadcast(array, source_image=1)
    end block

  contains

    pure function logical_and(op1, op2) result(res)
      logical, intent(in) :: op1, op2
      logical :: res

      res = op1 .and. op2

    end function logical_and

  end subroutine coa_env_reduce_and


  !> Broadcast an allocatable character.
  subroutine coa_env_broadcast_alloc_char(this, buffer, source)

    !> Instance
    class(coa_env), intent(in) :: this

    !> Buffer to send (can be unallocated)
    character(:), allocatable, intent(inout) :: buffer

    !> Source rank
    integer, intent(in) :: source

    integer :: charlen, stat
    character(100) :: msg

    charlen = -1
    if (this%image == source .and. allocated(buffer)) charlen = len(buffer)
    call co_broadcast(charlen, source)
    ! if (stat /= 0) error stop "co_broadcast(1) failed in coa_env_broadcast_alloc_char " // trim(msg)
    if (this%image /= source) then
      if (allocated(buffer)) deallocate(buffer)
      if (charlen >= 0) allocate(character(charlen) :: buffer)
    end if
    if (charlen > 0) then
      call co_broadcast(buffer, source)
      ! if (stat /= 0) error stop "co_broadcast(2) failed in coa_env_broadcast_alloc_char " // trim(msg)
    end if

  end subroutine coa_env_broadcast_alloc_char

end module fortuno_coarray_coaenv
