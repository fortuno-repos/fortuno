# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

project(
  'fortuno',
  'fortran',
  version: '0.1.0',
)

with_serial = get_option('with_serial')
with_mpi = get_option('with_mpi')
with_coarray = get_option('with_coarray')
with_examples = get_option('with_examples')

fortuno_sources = []
fortuno_serial_sources = []
fortuno_mpi_sources = []
fortuno_coarray_sources = []
subdir('src')

fflags_threadsafe = get_option('fflags_threadsafe')
ldflags_threadsafe = get_option('ldflags_threadsafe')
fflags_coarray = get_option('fflags_coarray')
ldflags_coarray = get_option('ldflags_coarray')

fortuno_lib = library(
  'fortuno',
  version: meson.project_version(),
  sources: fortuno_sources,
  fortran_args: fflags_threadsafe,
  link_args: ldflags_threadsafe,
)

fortuno_dep = declare_dependency(
  link_with: fortuno_lib,
  include_directories: 'include',
)

if with_serial

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

if with_mpi

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

if with_coarray
  fortuno_coarray_deps = [fortuno_dep]

  # Disabling coarray detection as it seems to be fragile. Make sure to pass the right coarray
  # compiler and linker arguments via the "fflags_coarray" and "ldflags_coarray" build variables.
  # coarray_dep = dependency('coarray')
  # fortuno_coarray_deps += coarray_dep

  fortuno_coarray_lib = library(
    'fortuno_coarray',
    version: meson.project_version(),
    sources: fortuno_coarray_sources,
    dependencies: fortuno_coarray_deps,
    fortran_args: fflags_coarray,
    link_args: ldflags_coarray,
  )

  fortuno_coarray_dep = declare_dependency(
    link_with: fortuno_coarray_lib,
    dependencies: fortuno_coarray_deps,
  )

endif

if with_examples
  subdir('example')
endif
