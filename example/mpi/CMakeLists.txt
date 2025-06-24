# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

cmake_minimum_required(VERSION 3.22...3.31)

project(mpi-example
    LANGUAGES Fortran
)

find_package(MPI MODULE COMPONENTS Fortran)
find_package(Fortuno CONFIG REQUIRED)

add_library(mylib)
target_sources(
  mylib PRIVATE
  mylib.f90
)
target_link_libraries(mylib PRIVATE MPI::MPI_Fortran)

add_executable(testapp)
target_sources(
  testapp PRIVATE
  test_simple.f90
  testapp.f90
)
target_link_libraries(
  testapp PRIVATE
  mylib Fortuno::fortuno_mpi MPI::MPI_Fortran
)

enable_testing()
add_test(NAME fortuno_test
    COMMAND testapp
)
