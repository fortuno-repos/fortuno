! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Interface module for the Fortuno testing framework with the coarray interface.
module fortuno_coarray
  use fortuno
  use fortuno_coarray_coabasetypes, only : coa_pure_case_base, coa_pure_suite_base
  use fortuno_coarray_coacmdapp, only : coa_cmd_app, execute_coa_cmd_app, init_coa_cmd_app,&
      & run_coa_cmd_app
  use fortuno_coarray_coacontext, only : coa_context
  use fortuno_coarray_coapurecase, only : coa_pure_case, coa_pure_case_item
  use fortuno_coarray_coapuresuite, only : coa_pure_suite, coa_pure_suite_item
  implicit none

end module fortuno_coarray
