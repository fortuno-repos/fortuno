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


# Defines the ThreadSafeBuild target for the thread-safe parts
function (fortuno_create_thread_safe_build_target)

  if (NOT TARGET ThreadSafeBuild)
    if (FORTUNO_THREAD_SAFE_COMPILE_FLAGS)
      set(_compiler_flags "${FORTUNO_THREAD_SAFE_COMPILE_FLAGS}")
    elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
      set(_compiler_flags "-thread_safe")
    endif ()

    if (FORTUNO_THREAD_SAFE_LINK_FLAGS)
      set(_linker_flags "${FORTUNO_THREAD_SAFE_LINK_FLAGS}")
    elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
      set(_linker_flags "-thread_safe")
    endif ()

    add_library(ThreadSafeBuild INTERFACE)
    target_compile_options(ThreadSafeBuild INTERFACE ${_compiler_flags})
    target_link_options(ThreadSafeBuild INTERFACE ${_linker_flags})
  endif ()

endfunction ()


# Applies thread safe build flags to a target
function (fortuno_add_thread_safe_build_flags target)

  fortuno_create_thread_safe_build_target()

  # TODO: Delete first branch once cmake minimum version is 3.26 or above
  if (CMAKE_VERSION VERSION_LESS 3.26)
    get_target_property(_compile_flags ThreadSafeBuild INTERFACE_COMPILE_OPTIONS)
    if (_compile_flags)
      target_compile_options(${target} PRIVATE ${_compile_flags})
    endif ()
    get_target_property(_link_flags ThreadSafeBuild INTERFACE_LINK_OPTIONS)
    if (_link_flags)
      target_link_options(${target} PRIVATE ${_link_flags})
    endif ()
  else ()
    target_link_libraries(${target} PRIVATE $<BUILD_LOCAL_INTERFACE:ThreadSafeBuild>)
  endif ()

endfunction ()


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


# Creates the target CoarrayBuildInterface with coarray build options
function (fortuno_create_coarray_build_target)

  if (NOT TARGET CoarrayBuildInterface)
    add_library(CoarrayBuildInterface INTERFACE)
    target_compile_options(
      CoarrayBuildInterface INTERFACE
      ${FORTUNO_COARRAY_COMPILE_FLAGS}
    )
    target_link_options(
      CoarrayBuildInterface INTERFACE
      ${FORTUNO_COARRAY_LINK_FLAGS}
    )
  endif ()

endfunction ()


# Applies coarray build flags to a target
function (fortuno_add_coarray_build_flags target)

  fortuno_create_coarray_build_target()

  # TODO: Delete first branch once cmake minimum version is 3.26 or above
  # Older CMake versions have problems during installation if the CoarrayBuildInterface target is
  # linked directly with any target, therefore applying a workaround.
  if (CMAKE_VERSION VERSION_LESS 3.26)
    get_target_property(_compile_flags CoarrayBuildInterface INTERFACE_COMPILE_OPTIONS)
    if (_compile_flags)
      target_compile_options(${target} PRIVATE ${_compile_flags})
    endif ()
    get_target_property(_link_flags CoarrayBuildInterface INTERFACE_LINK_OPTIONS)
    if (_link_flags)
      target_link_options(${target} PRIVATE ${_link_flags})
    endif ()
  else ()
    target_link_libraries(${target} PRIVATE $<BUILD_LOCAL_INTERFACE:CoarrayBuildInterface>)
  endif ()

endfunction ()
