# This file is part of Fortuno.
# Licensed under the BSD-2-Clause Plus Patent license.
# SPDX-License-Identifier: BSD-2-Clause-Patent

function(Fortuno_add_test test)
  #[===[.md
  # Fortuno_add_test

  Internal helper for adding functional tests testing whole CMake projects.

  ## Synopsis
  ```cmake
  Template_add_test(<name>
    [TEST_NAME <test_name>]
  )
  ```

  ## Options

  `<name>`
  : Path to the CMake project to be executed relative to `${CMAKE_CURRENT_SOURCE_DIR}`

  `TEST_NAME` [Default: `<name>`]
  : Name for the test to be used as the ctest name
  ]===]

  set(ARGS_Options)
  set(ARGS_OneValue
    TEST_NAME
  )
  set(ARGS_MultiValue)
  cmake_parse_arguments(PARSE_ARGV 1 ARGS "${ARGS_Options}" "${ARGS_OneValue}" "${ARGS_MultiValue}")

  # Check required/optional arguments
  if (ARGC LESS 1)
    message(FATAL_ERROR "Missing test name")
  endif ()
  if (NOT DEFINED ARGS_TEST_NAME)
    set(ARGS_TEST_NAME ${test})
  endif ()

  set(configure_args
    -DCMAKE_Fortran_COMPILER=${CMAKE_Fortran_COMPILER}
  )
  if (Fortuno_IS_TOP_LEVEL)
    list(APPEND configure_args
      # Generated Config file point to binary targets until it is installed
      -DFortuno_ROOT=${Fortuno_BINARY_DIR}
      -DFETCHCONTENT_SOURCE_DIR_FORTUNO=${Template_SOURCE_DIR}
    )
  endif ()

  add_test(NAME ${ARGS_TEST_NAME}
    COMMAND ${CMAKE_CTEST_COMMAND} --build-and-test ${CMAKE_CURRENT_SOURCE_DIR}/${test}
    ${CMAKE_CURRENT_BINARY_DIR}/${test}
    # Use the same build environment as the current runner
    --build-generator "${CMAKE_GENERATOR}"
    --build-options ${configure_args}
    --test-command ${CMAKE_CTEST_COMMAND}
      --test-dir ${CMAKE_CURRENT_BINARY_DIR}/${test}
      --output-on-failure
  )
endfunction()
