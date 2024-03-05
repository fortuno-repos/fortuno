# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

### Project definition ###

project(
  'fortuno_test_export',
  'fortran',
  version: '0.0.0',
)

### Getting dependencies ###

testapp_deps = []

fortuno_dep = dependency('fortuno', fallback: ['fortuno', 'fortuno_dep'])
testapp_deps += fortuno_dep

### Building the app ###

testapp_srcs = []
subdir('app')

testapp_exe = executable(
  'testapp',
  sources: testapp_srcs,
  dependencies: testapp_deps,
)
