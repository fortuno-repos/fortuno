# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

target_sources(
  fortuno PRIVATE
  argumentparser.f90
  basetypes.f90
  checkers.f90
  cmdapp.f90
  consolelogger.f90
  namedtypes.f90
  testcontext.f90
  testdriver.f90
  testinfo.f90
  testlogger.f90
  utils.f90
  version.f90
)
