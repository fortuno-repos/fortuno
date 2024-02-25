# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

project(
  'fortuno',
  'fortran',
  version: '0.1.0',
)

sources = []
subdir('src')

fortuno_lib = library(
  meson.project_name(),
  sources: sources,
  version: meson.project_version(),
)

fortuno_dep = declare_dependency(
  link_with: fortuno_lib,
)

build_examples = get_option('build_examples')
if build_examples
  subdir('example')
endif
