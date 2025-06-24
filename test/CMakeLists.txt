# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

list(APPEND CMAKE_MESSAGE_CONTEXT Test)

include(FortunoTestHelpers)

add_subdirectory(unit)
add_subdirectory(example)
