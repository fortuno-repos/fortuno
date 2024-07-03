# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

list(APPEND CMAKE_MESSAGE_CONTEXT Fypp)

add_library(fortuno_example_fypp_mylib)
set_target_properties(
  fortuno_example_fypp_mylib PROPERTIES
  OUTPUT_NAME mylib
)
target_sources(
  fortuno_example_fypp_mylib PRIVATE
  mylib.f90
)

add_executable(fortuno_example_fypp_testapp)
set_target_properties(
  fortuno_example_fypp_testapp PROPERTIES
  OUTPUT_NAME testapp
)

set(
  fypp-sources
  fixtured_fypp_tests.fypp
  parametrized_fypp_tests.fypp
  simple_fypp_tests.fypp
)
fortuno_preprocess(
  ${FYPP} "-I${CMAKE_SOURCE_DIR}/include;--file-var-root=${CMAKE_SOURCE_DIR}"
  .fypp .f90
  "${fypp-sources}" fypp-f90-sources
)

target_sources(
  fortuno_example_fypp_testapp PRIVATE
  ${fypp-f90-sources}
  testapp.f90
)
target_link_libraries(
  fortuno_example_fypp_testapp
  PRIVATE
  fortuno_example_fypp_mylib Fortuno::fortuno_serial
)
