# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

list(APPEND CMAKE_MESSAGE_CONTEXT Example)

add_library(Fortuno_example_mylib)
set_target_properties(
  Fortuno_example_mylib PROPERTIES
  OUTPUT_NAME mylib
)
target_sources(
  Fortuno_example_mylib PRIVATE
  mylib.f90
)

add_executable(Fortuno_example_testapp)
set_target_properties(
  Fortuno_example_testapp PROPERTIES
  OUTPUT_NAME testapp
)
target_sources(
  Fortuno_example_testapp PRIVATE
  fixtured_tests.f90
  simple_tests.f90
  testapp.f90
)
target_link_libraries(Fortuno_example_testapp PRIVATE Fortuno_example_mylib Fortuno::Fortuno)
