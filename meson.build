# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

project(
  'fortuno',
  'fortran',
  version: '0.1.0',
)

build_serial_interface = get_option('build_serial_interface')
build_examples = get_option('build_examples')

fortuno_sources = []
fortuno_serial_sources = []
subdir('src')

thread_safe_flags = get_option('thread_safe_flags')
thread_safe_link_flags = get_option('thread_safe_link_flags')

fortuno_lib = library(
  'fortuno',
  version: meson.project_version(),
  sources: fortuno_sources,
  fortran_args: thread_safe_flags,
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

if build_serial_interface and build_examples
  subdir('example')
endif
