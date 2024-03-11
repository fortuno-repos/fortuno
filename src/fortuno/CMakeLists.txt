# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent


target_sources(
  Fortuno PRIVATE
  base.f90
  serial.f90
)
add_subdirectory(base)
add_subdirectory(serial)
