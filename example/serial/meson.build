# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

example_mylib_lib = library(
  'mylib',
  sources: ['mylib.f90'],
  install: false
)
example_mylib_dep = declare_dependency(
  link_with: example_mylib_lib,
)

example_testapp_exe = executable(
  'testapp',
  sources: [
    'test_fixtured_module.f90',
    'test_fixtured_suite.f90',
    'test_fixtured.f90',
    'test_parametrized.f90',
    'test_simple.f90',
    'testapp.f90',
  ],
  dependencies: [example_mylib_dep, fortuno_serial_dep],
  install: false,
)
