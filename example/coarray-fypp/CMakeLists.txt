# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

cmake_minimum_required(VERSION 3.22...3.31)

project(coarray-fypp-example
    LANGUAGES Fortran
)

find_package(Fortuno CONFIG REQUIRED)

find_program(FYPP fypp)

# TODO: decouple this from config.cmake setup
# TODO: Do not hardcode the path dependence
# Get the absolute path of the Fortuno repo
cmake_path(SET Fortuno_Source_Dir NORMALIZE "${CMAKE_CURRENT_SOURCE_DIR}/../../..")
include(${Fortuno_Source_Dir}/config.cmake)
include(${Fortuno_Source_Dir}/cmake/FortunoHelpers.cmake)

add_library(mylib)
target_sources(
  mylib PRIVATE
  mylib.f90
)
target_compile_options(mylib PRIVATE ${FORTUNO_FFLAGS_COARRAY})
target_link_options(mylib PRIVATE ${FORTUNO_LDFLAGS_COARRAY})

add_executable(testapp)

set(
  fypp-sources
  test_simple_fypp.fypp
)

get_target_property(
  _fortuno_incdir
  Fortuno::fortuno_include_dir
  INTERFACE_INCLUDE_DIRECTORIES
)
fortuno_preprocess(
  ${FYPP} "-I${_fortuno_incdir};--file-var-root=${Fortuno_Source_Dir}"
  .fypp .f90
  "${fypp-sources}" fypp-f90-sources
)

target_sources(
  testapp PRIVATE
  ${fypp-f90-sources}
  testapp.f90
)
target_link_libraries(
  testapp
  PRIVATE
  mylib Fortuno::fortuno_coarray
)
target_compile_options(testapp PRIVATE ${FORTUNO_FFLAGS_COARRAY})
target_link_options(testapp PRIVATE ${FORTUNO_LDFLAGS_COARRAY})

enable_testing()
add_test(NAME fortuno_test
    COMMAND testapp
)
