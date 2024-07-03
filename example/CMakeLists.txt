# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

list(APPEND CMAKE_MESSAGE_CONTEXT Example)

add_library(fortuno_example_mylib)
set_target_properties(
  fortuno_example_mylib PROPERTIES
  OUTPUT_NAME mylib
)
target_sources(
  fortuno_example_mylib PRIVATE
  mylib.f90
)

add_executable(fortuno_example_testapp)
set_target_properties(
  fortuno_example_testapp PROPERTIES
  OUTPUT_NAME testapp
)
target_sources(
  fortuno_example_testapp PRIVATE
  fixtured_module_tests.f90
  fixtured_suite_tests.f90
  fixtured_tests.f90
  parametrized_tests.f90
  simple_tests.f90
  testapp.f90
)
target_link_libraries(fortuno_example_testapp PRIVATE fortuno_example_mylib Fortuno::fortuno_serial)
