# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

### Project definition ###

project(
  'fortuno_coarray_test_export',
  'fortran',
  version: '0.0.0',
)

### Getting dependencies ###

fflags_coarray = get_option('fflags_coarray')
ldflags_coarray = get_option('ldflags_coarray')

testapp_deps = []

fortuno_coarray_dep = dependency(
  'fortuno-coarray',
  fallback: ['fortuno', 'fortuno_coarray_dep'],
  default_options: {'fflags_coarray': fflags_coarray, 'ldflags_coarray': ldflags_coarray},
)
testapp_deps += fortuno_coarray_dep

### Building the app ###

testapp_srcs = []
subdir('app')

testapp_exe = executable(
  'testapp',
  sources: testapp_srcs,
  dependencies: testapp_deps,
  fortran_args: fflags_coarray,
  link_args: ldflags_coarray,
)
