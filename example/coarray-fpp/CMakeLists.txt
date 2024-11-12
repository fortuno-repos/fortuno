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
target_compile_options(fortuno_example_coarray_fpp_mylib PRIVATE ${FORTUNO_FFLAGS_COARRAY})
target_link_options(fortuno_example_coarray_fpp_mylib PRIVATE ${FORTUNO_LDFLAGS_COARRAY})

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
target_compile_options(fortuno_example_coarray_fpp_testapp PRIVATE ${FORTUNO_FFLAGS_FPP})
target_link_libraries(
  fortuno_example_coarray_fpp_testapp PRIVATE
  fortuno_example_coarray_fpp_mylib
  Fortuno::fortuno_coarray
)
target_compile_options(fortuno_example_coarray_fpp_testapp PRIVATE ${FORTUNO_FFLAGS_COARRAY})
target_link_options(fortuno_example_coarray_fpp_testapp PRIVATE ${FORTUNO_LDFLAGS_COARRAY})
