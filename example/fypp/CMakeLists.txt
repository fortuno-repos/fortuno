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
  OUTPUT_NAME testapp_fypp
)

set(
  fypp-sources
  test_fixtured_fypp.fypp
  test_parametrized_fypp.fypp
  test_simple_fypp.fypp
)

get_target_property(_fortuno_incdir Fortuno::fortuno_include_dir INTERFACE_INCLUDE_DIRECTORIES)
fortuno_preprocess(
  ${FYPP} "-I${_fortuno_incdir};--file-var-root=${CMAKE_SOURCE_DIR}"
  .fypp .f90
  "${fypp-sources}" fypp-f90-sources
)

target_sources(
  fortuno_example_fypp_testapp PRIVATE
  ${fypp-f90-sources}
  testapp_fypp.f90
)
target_link_libraries(
  fortuno_example_fypp_testapp
  PRIVATE
  fortuno_example_fypp_mylib Fortuno::fortuno_serial
)
