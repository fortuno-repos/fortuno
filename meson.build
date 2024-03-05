# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

### Project definition ###

project(
  'fortuno',
  'fortran',
  version: '0.1.0',
)

### Building the library ###

fortuno_srcs = []
subdir('src')

fortuno_lib = library(
  meson.project_name(),
  version: meson.project_version(),
  sources: fortuno_srcs,
)

### Exporting the library ###

fortuno_dep = declare_dependency(
  link_with: fortuno_lib,
)

### Building optional components ###

build_examples = get_option('build_examples')
if build_examples
  subdir('example')
endif
