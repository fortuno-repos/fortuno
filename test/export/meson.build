# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

project(
  'fortuno_test_export',
  'fortran',
  version: '0.0.0',
)

testapp_deps = []

fortuno_serial_dep = dependency('fortuno-serial', fallback: ['fortuno', 'fortuno_serial_dep'])
testapp_deps += fortuno_serial_dep

testapp_sources = []
subdir('app')

testapp_exe = executable(
  'testapp',
  sources: testapp_sources,
  dependencies: testapp_deps,
)
