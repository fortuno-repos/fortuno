# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

list(APPEND CMAKE_MESSAGE_CONTEXT Mpi)

add_library(fortuno_example_mpi_mylib)
set_target_properties(
  fortuno_example_mpi_mylib PROPERTIES
  OUTPUT_NAME mylib
)
target_sources(
  fortuno_example_mpi_mylib PRIVATE
  mylib.f90
)
target_link_libraries(fortuno_example_mpi_mylib PRIVATE MPI::MPI_Fortran)

add_executable(fortuno_example_mpi_testapp)
set_target_properties(
  fortuno_example_mpi_testapp PROPERTIES
  OUTPUT_NAME testapp
)
target_sources(
  fortuno_example_mpi_testapp PRIVATE
  test_simple.f90
  testapp.f90
)
target_link_libraries(
  fortuno_example_mpi_testapp PRIVATE
  fortuno_example_mpi_mylib Fortuno::fortuno_mpi MPI::MPI_Fortran
)
