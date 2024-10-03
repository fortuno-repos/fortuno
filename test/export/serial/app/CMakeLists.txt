# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

add_executable(testapp)
target_sources(testapp PRIVATE testapp.f90)
target_link_libraries(testapp PRIVATE Fortuno::fortuno_serial)

add_executable(testapp_fpp)
target_sources(testapp_fpp PRIVATE testapp_fpp.F90)
target_link_libraries(testapp_fpp PRIVATE Fortuno::fortuno_serial)

if (FYPP)
  add_executable(testapp_fypp)

  set(_oldfile "testapp_fypp.fypp")
  string(REGEX REPLACE ".fypp$" ".f90" _newfile "${_oldfile}")
  add_custom_command(
    OUTPUT ${_newfile}
    COMMAND ${FYPP} ${FYPP_INCOPTS} ${CMAKE_CURRENT_SOURCE_DIR}/${_oldfile} ${_newfile}
    MAIN_DEPENDENCY ${CMAKE_CURRENT_SOURCE_DIR}/${_oldfile}
  )
  target_sources(testapp_fypp PRIVATE ${_newfile})
  target_link_libraries(testapp_fypp PRIVATE Fortuno::fortuno_serial)
endif ()
