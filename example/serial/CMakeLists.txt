# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

cmake_minimum_required(VERSION 3.22...3.31)

project(serial-example
    LANGUAGES Fortran
)

find_package(Fortuno CONFIG REQUIRED)

add_library(mylib)
target_sources(
  mylib PRIVATE
  mylib.f90
)

add_executable(testapp)
target_sources(
  testapp PRIVATE
  testapp.f90
  test_fixtured.f90
  test_fixtured_module.f90
  test_fixtured_suite.f90
  test_parametrized.f90
  test_simple.f90
)
target_link_libraries(
  testapp PRIVATE mylib Fortuno::fortuno_serial
)

enable_testing()
add_test(NAME fortuno_test
    COMMAND testapp
)
