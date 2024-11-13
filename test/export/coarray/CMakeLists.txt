# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

cmake_minimum_required(VERSION 3.22...3.31)

project(
  FortunoCoarray_Test_Export
  VERSION 0.0.0
  DESCRIPTION "Testing the CMake build info exported by Fortuno"
  LANGUAGES Fortran
)

find_package(Fortuno 0.1.0 EXACT REQUIRED)
if (NOT TARGET Fortuno::fortuno_coarray)
  message(FATAL_ERROR "Fortuno library was built without the coarray interface")
endif ()

find_program(FYPP fypp)
if (FYPP)
  get_target_property(
    _fortuno_incdir
    Fortuno::fortuno_include_dir
    INTERFACE_INCLUDE_DIRECTORIES
  )
  set(FYPP_INCOPTS "-I${_fortuno_incdir}")
endif ()

add_subdirectory(app)
