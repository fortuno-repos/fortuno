# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

example_mylib_lib = library(
  'mylib',
  sources: ['mylib.f90'],
  dependencies: [mpi_fortran_dep],
  install: false,
)
example_mylib_dep = declare_dependency(
  link_with: example_mylib_lib,
)

example_testapp_exe = executable(
  'testapp',
  sources: [
    'test_simple.f90',
    'testapp.f90',
  ],
  dependencies: [example_mylib_dep, fortuno_mpi_dep],
  install: false,
)
