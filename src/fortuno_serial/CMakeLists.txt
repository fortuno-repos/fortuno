# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

target_sources(
  fortuno_serial PRIVATE
  serialbasetypes.f90
  serialcase.f90
  serialcmdapp.f90
  serialconlogger.f90
  serialcontext.f90
  serialdriver.f90
  serialglobalctx.f90
  serialsuite.f90
)
