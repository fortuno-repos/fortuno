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
function (fortuno_def_thread_safe_build_target)

  if (FORTUNO_THREAD_SAFE_FLAGS)
    set(_compiler_flags "${FORTUNO_THREAD_SAFE_FLAGS}")
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

endfunction ()