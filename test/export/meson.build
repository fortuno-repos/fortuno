project(
  'fortuno_test_export',
  'fortran',
  version: '0.0',
)

if get_option('fortuno_subproject')
  fortuno_subproject = subproject('fortuno')
  fortuno_dep = fortuno_subproject.get_variable('fortuno_dep')
else
  fortuno_dep = dependency('fortuno')
endif

sources = []
subdir('app')

testapp_exe = executable(
  'testapp',
  sources: sources,
  dependencies: [fortuno_dep],
)
