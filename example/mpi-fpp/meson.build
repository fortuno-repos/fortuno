# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

example_mylib_fpp_lib = library(
  'mylib',
  sources: ['mylib.f90'],
  dependencies: [mpi_fortran_dep],
  install: false,
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
  dependencies: [example_mylib_fpp_dep, fortuno_mpi_dep],
  install: false,
)
