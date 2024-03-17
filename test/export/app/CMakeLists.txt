# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

target_sources(testapp PRIVATE testapp.f90)
target_link_libraries(testapp PRIVATE Fortuno::fortuno_serial)
