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

coarray_compile_flags = get_option('coarray_compile_flags')
coarray_link_flags = get_option('coarray_link_flags')

testapp_deps = []

fortuno_coarray_dep = dependency(
  'fortuno-coarray',
  fallback: ['fortuno', 'fortuno_coarray_dep'],
  default_options: {'coarray_compile_flags': coarray_compile_flags, 'coarray_link_flags': coarray_link_flags},
)
testapp_deps += fortuno_coarray_dep

### Building the app ###

testapp_srcs = []
subdir('app')

testapp_exe = executable(
  'testapp',
  sources: testapp_srcs,
  dependencies: testapp_deps,
  fortran_args: coarray_compile_flags,
  link_args: coarray_link_flags,
)
