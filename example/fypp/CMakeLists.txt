# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

function (fortuno_preprocess preproc preprocopts oldext newext oldfiles newfiles)

  foreach (oldfile IN LISTS oldfiles)
    # Start with an absolulte path, so that the correct relative path is calculated thereafter
    get_filename_component(oldfile ${oldfile} ABSOLUTE ${CMAKE_CURRENT_SOURCE_DIR})
    file(RELATIVE_PATH oldfile ${CMAKE_CURRENT_SOURCE_DIR} ${oldfile})
    string(REGEX REPLACE "\\${oldext}$" "${newext}" _newfile "${oldfile}")
    add_custom_command(
      OUTPUT ${CMAKE_CURRENT_BINARY_DIR}/${_newfile}
      COMMAND ${preproc} ${preprocopts} ${CMAKE_CURRENT_SOURCE_DIR}/${oldfile} ${CMAKE_CURRENT_BINARY_DIR}/${_newfile}
      MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${oldfile})
    list(APPEND _newfiles ${CMAKE_CURRENT_BINARY_DIR}/${_newfile})
  endforeach ()
  set(${newfiles} "${_newfiles}" PARENT_SCOPE)

endfunction ()


list(APPEND CMAKE_MESSAGE_CONTEXT Fypp)

add_library(fortuno_example_fypp_mylib)
set_target_properties(
  fortuno_example_fypp_mylib PROPERTIES
  OUTPUT_NAME mylib
)
target_sources(
  fortuno_example_fypp_mylib PRIVATE
  mylib.f90
)

add_executable(fortuno_example_fypp_testapp)
set_target_properties(
  fortuno_example_fypp_testapp PROPERTIES
  OUTPUT_NAME testapp
)

set(
  fypp-sources
  fixtured_fypp_tests.fypp
  parametrized_fypp_tests.fypp
  simple_fypp_tests.fypp
)
fortuno_preprocess(
  ${FYPP} "-I${CMAKE_SOURCE_DIR}/include;--file-var-root=${CMAKE_SOURCE_DIR}"
  .fypp .f90
  "${fypp-sources}" fypp-f90-sources
)

target_sources(
  fortuno_example_fypp_testapp PRIVATE
  ${fypp-f90-sources}
  testapp.f90
)
target_link_libraries(
  fortuno_example_fypp_testapp
  PRIVATE
  fortuno_example_fypp_mylib Fortuno::fortuno_serial
)
