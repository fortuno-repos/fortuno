! This file is part of Fortuno.
! Licensed under the BSD-2-Clause Plus Patent license.
! SPDX-License-Identifier: BSD-2-Clause-Patent

!> Interface module for the core library of the Fortuno testing framework
module fortuno
  use fortuno_basetypes, only : test_base, test_case_base, test_item, test_list, test_ptr_item,&
      & test_suite_base
  use fortuno_chartypes, only : stringable, details_dict, dict_item, state_dict,&
      & matches_type_value, get_ptr_to
  use fortuno_consolelogger, only : console_logger
  use fortuno_env, only : nl
  use fortuno_testcontext, only : context_factory, test_context
  use fortuno_checkers, only : all_close, all_equal, is_close, is_equal
  use fortuno_cmdapp, only : cmd_app
  use fortuno_testdriver, only : init_test_driver, test_driver, test_runner, test_selection
  use fortuno_testinfo, only : check_result, drive_result, failure_info, failure_location,&
      & init_drive_result, init_failure_location, test_result, teststatus
  use fortuno_utils, only : str
  implicit none

end module fortuno
