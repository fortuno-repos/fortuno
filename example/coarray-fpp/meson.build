# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

example_mylib_fpp_lib = library(
  'mylib',
  sources: ['mylib.f90'],
  install: false,
  fortran_args: fflags_coarray,
  link_args: ldflags_coarray,
)
example_mylib_fpp_dep = declare_dependency(
  link_with: example_mylib_fpp_lib,
)

example_testapp_exe = executable(
  'testapp',
  sources: [
    'test_simple_fpp.F90',
    'testapp.f90',
  ],
  dependencies: [example_mylib_fpp_dep, fortuno_coarray_dep],
  install: false,
  fortran_args: fflags_coarray,
  link_args: ldflags_coarray,
)
