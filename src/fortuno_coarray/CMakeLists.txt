# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

target_sources(
  fortuno_coarray
  PRIVATE
  coabasetypes.f90
  coacmdapp.f90
  coaconlogger.f90
  coacontext.f90
  coadriver.f90
  coaenv.f90
  coatestinfo.f90
  coapurecase.f90
  coapuresuite.f90
)
