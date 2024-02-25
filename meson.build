project(
  'fortuno',
  'fortran',
  version: '0.1.0',
)

sources = []
subdir('src')

# Intel compiler (as of 2024.0.2) crashes when building the library with optimization turned on.
if meson.get_compiler('fortran').get_id() == 'intel-llvm'
  fortran_args = ['-O0']
else
  fortran_args = []
endif

fortuno_lib = library(
  meson.project_name(),
  sources: sources,
  version: meson.project_version(),
  fortran_args: fortran_args
)

fortuno_dep = declare_dependency(
  link_with: fortuno_lib,
)

build_examples = get_option('build_examples')
if build_examples
  subdir('example')
endif
