# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

target_sources(
  fortuno PRIVATE
  helpers.f90
  int32.f90
  int64.f90
  real32.f90
  real64.f90
)

# Make sure all compilers find the include files used in this directory
target_include_directories(fortuno PRIVATE ${CMAKE_CURRENT_SOURCE_DIR})