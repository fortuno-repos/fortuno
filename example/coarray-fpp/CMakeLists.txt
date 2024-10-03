# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

list(APPEND CMAKE_MESSAGE_CONTEXT CoarrayFpp)

add_library(fortuno_example_coarray_fpp_mylib)
set_target_properties(
  fortuno_example_coarray_fpp_mylib PROPERTIES
  OUTPUT_NAME mylib
)
target_sources(
  fortuno_example_coarray_fpp_mylib PRIVATE
  mylib.f90
)
fortuno_add_coarray_build_flags(fortuno_example_coarray_fpp_mylib)

add_executable(fortuno_example_coarray_fpp_testapp)
set_target_properties(
  fortuno_example_coarray_fpp_testapp PROPERTIES
  OUTPUT_NAME testapp
)
target_sources(
  fortuno_example_coarray_fpp_testapp PRIVATE
  test_simple_fpp.F90
  testapp.f90
)
target_link_libraries(
  fortuno_example_coarray_fpp_testapp PRIVATE
  fortuno_example_coarray_fpp_mylib
  Fortuno::fortuno_coarray
)
fortuno_add_coarray_build_flags(fortuno_example_coarray_fpp_testapp)
