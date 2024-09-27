# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

list(APPEND CMAKE_MESSAGE_CONTEXT Unit)

add_executable(fortuno_test_unit_testapp)
set_target_properties(
  fortuno_test_unit_testapp PROPERTIES
  OUTPUT_NAME testapp
)
target_sources(
  fortuno_test_unit_testapp PRIVATE
  test_checkers.f90
  testapp.f90
)
target_link_libraries(fortuno_test_unit_testapp PRIVATE Fortuno::fortuno_serial)
add_test(NAME unit COMMAND testapp)
