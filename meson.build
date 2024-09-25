# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

project(
  'fortuno',
  'fortran',
  version: '0.1.0',
)

build_serial_interface = get_option('build_serial_interface')
build_mpi_interface = get_option('build_mpi_interface')
build_examples = get_option('build_examples')

fortuno_sources = []
fortuno_serial_sources = []
fortuno_mpi_sources = []
subdir('src')

thread_safe_compile_flags = get_option('thread_safe_compile_flags')
thread_safe_link_flags = get_option('thread_safe_link_flags')

fortuno_lib = library(
  'fortuno',
  version: meson.project_version(),
  sources: fortuno_sources,
  fortran_args: thread_safe_compile_flags,
  link_args: thread_safe_link_flags,
)

fortuno_dep = declare_dependency(
  link_with: fortuno_lib,
)

if build_serial_interface

  fortuno_serial_deps = [fortuno_dep]

  fortuno_serial_lib = library(
    'fortuno_serial',
    version: meson.project_version(),
    sources: fortuno_serial_sources,
    dependencies: fortuno_serial_deps,
  )

  fortuno_serial_dep = declare_dependency(
    link_with: fortuno_serial_lib,
    dependencies: fortuno_serial_deps,
  )

endif

if build_mpi_interface

  fortuno_mpi_deps = [fortuno_dep]

  mpi_fortran_dep = dependency('mpi', language : 'fortran', required: true)
  fortuno_mpi_deps += mpi_fortran_dep

  fortuno_mpi_lib = library(
    'fortuno_mpi',
    version: meson.project_version(),
    sources: fortuno_mpi_sources,
    dependencies: fortuno_mpi_deps,
  )

  fortuno_mpi_dep = declare_dependency(
    link_with: fortuno_mpi_lib,
    dependencies: fortuno_mpi_deps,
  )

endif

if build_examples
  subdir('example')
endif
