# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

# Sets up the build type.
function (fortuno_setup_build_type default_build_type)

  get_property(_multiConfig GLOBAL PROPERTY GENERATOR_IS_MULTI_CONFIG)
  if(_multiConfig)
    message(STATUS "Build type multi-config (build type selected at the build step)")
  else()
    if(NOT CMAKE_BUILD_TYPE)
      message(STATUS "Build type ${default_build_type} (default single-config)")
      set(CMAKE_BUILD_TYPE "${default_build_type}" CACHE STRING "Build type" FORCE)
      set_property(CACHE CMAKE_BUILD_TYPE PROPERTY HELPSTRING "Choose the type of build")
      set_property(
        CACHE CMAKE_BUILD_TYPE PROPERTY STRINGS "Debug" "Release" "RelWithDebInfo"
      )
    else()
      message(STATUS "Fortuno: build type: ${CMAKE_BUILD_TYPE} (manually selected single-config)")
    endif()
  endif()

endfunction()


# Preprocesses source files
#
# Args:
#     preproc: preprocessor to use (e.g. Fypp)
#     preprocopts: options to pass to the preprocessor (apart of input and output files)
#     oldext: extension of the files to pre-process (e.g. .fypp or .F90)
#     newext: extension of the pre-processed source files (e.g. .f90)
#     oldfiles: list of the files to preprocess
#     newfiles [out]: variable which should contain the list of the pre-processed files on return
#
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
