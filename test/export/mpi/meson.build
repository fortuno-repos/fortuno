# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

project(
  'fortuno_mpi_test_export',
  'fortran',
  version: '0.0.0',
)

testapp_deps = []
testapp_fpp_deps = []

fortran_mpi_dep = dependency('mpi', language: 'fortran', required: true)
testapp_deps += fortran_mpi_dep
testapp_fpp_deps += fortran_mpi_dep

fortuno_mpi_dep = dependency('fortuno_mpi', fallback: ['fortuno', 'fortuno_mpi_dep'])
testapp_deps += fortuno_mpi_dep
testapp_fpp_deps += fortuno_mpi_dep

testapp_srcs = []
testapp_fpp_srcs = []
subdir('app')

testapp_exe = executable(
  'testapp',
  sources: testapp_srcs,
  dependencies: testapp_deps,
)
testapp_exe = executable(
  'testapp_fpp',
  sources: testapp_fpp_srcs,
  dependencies: testapp_fpp_deps,
)
