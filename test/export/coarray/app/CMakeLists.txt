# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

add_executable(testapp)
target_sources(testapp PRIVATE testapp.f90)
target_link_libraries(testapp PRIVATE Fortuno::fortuno_coarray)

if (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
  target_compile_options(testapp PRIVATE "-coarray")
  target_link_options(testapp PRIVATE "-coarray")
elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")
  target_compile_options(testapp PRIVATE "-coarray")
  target_link_options(testapp PRIVATE "-coarray")
endif ()