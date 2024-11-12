# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

list(APPEND CMAKE_MESSAGE_CONTEXT SerialFpp)

add_library(fortuno_example_serial_fpp_mylib)
set_target_properties(
  fortuno_example_serial_fpp_mylib PROPERTIES
  OUTPUT_NAME mylib
)
target_sources(
  fortuno_example_serial_fpp_mylib PRIVATE
  mylib.f90
)

add_executable(fortuno_example_serial_fpp_testapp)
set_target_properties(
  fortuno_example_serial_fpp_testapp PROPERTIES
  OUTPUT_NAME testapp
)
target_sources(
  fortuno_example_serial_fpp_testapp PRIVATE
  testapp.f90
  test_fixtured_fpp.F90
  test_fixtured_module_fpp.F90
  test_fixtured_suite_fpp.F90
  test_parametrized_fpp.F90
  test_simple_fpp.F90
)
target_compile_options(fortuno_example_serial_fpp_testapp PRIVATE ${FORTUNO_FFLAGS_FPP})
target_link_libraries(
  fortuno_example_serial_fpp_testapp
  PRIVATE
  fortuno_example_serial_fpp_mylib
  Fortuno::fortuno_serial
)
