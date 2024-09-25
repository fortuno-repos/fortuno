# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

list(APPEND CMAKE_MESSAGE_CONTEXT Serial)

add_library(fortuno_example_serial_mylib)
set_target_properties(
  fortuno_example_serial_mylib PROPERTIES
  OUTPUT_NAME mylib
)
target_sources(
  fortuno_example_serial_mylib PRIVATE
  mylib.f90
)

add_executable(fortuno_example_serial_testapp)
set_target_properties(
  fortuno_example_serial_testapp PROPERTIES
  OUTPUT_NAME testapp
)
target_sources(
  fortuno_example_serial_testapp PRIVATE
  testapp.f90
  test_fixtured.f90
  test_fixtured_module.f90
  test_fixtured_suite.f90
  test_parametrized.f90
  test_simple.f90
)
target_link_libraries(
  fortuno_example_serial_testapp PRIVATE fortuno_example_serial_mylib Fortuno::fortuno_serial
)
