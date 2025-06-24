# This file contains all the customizable build configuration options. You can override them via the
# appropriate -D command line options or by creating a custom file with your settings and preloading
# it via the -C option.


# General options

option(FORTUNO_BUILD_SHARED_LIBS "Fortuno: Build as shared library" ${PROJECT_IS_TOP_LEVEL})

option(FORTUNO_WITH_SERIAL "Fortuno: whether the library with serial interface should be built" ON)

option(FORTUNO_WITH_MPI "Fortuno: whether the library with MPI interface should be built" OFF)

option(FORTUNO_WITH_COARRAY "Fortuno: whether library with coarray interface should be built" OFF)

option(FORTUNO_WITH_TESTS "Fortuno: whether to build test suite" ${PROJECT_IS_TOP_LEVEL})

if (FORTUNO_WITH_EXAMPLES)
  message(WARNING
      "FORTUNO_WITH_EXAMPLES is deprecated and has no effect. "
      "This is now part of the tests"
  )
endif ()

option(FORTUNO_INSTALL "Fortuno: Install project" ${PROJECT_IS_TOP_LEVEL})

set(
  FORTUNO_INSTALL_MODULEDIR "modules" CACHE STRING
  "Fortuno: Sub-directory for installed Fortran module files (relative to CMAKE_INSTALL_LIBDIR)"
)


# Compiler dependent options

if (CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")

  # Default settings for the Intel LLVM (ifx) compiler

  set(
    FORTUNO_FFLAGS_THREADSAFE ""
    CACHE STRING "Fortuno: Flags needed to enforce thread-safe build during compilation"
  )
  set(
    FORTUNO_LDFLAGS_THREADSAFE ""
    CACHE STRING  "Fortuno: Flags needed to enforce thread-safe build during linking"
  )
  set(
    FORTUNO_FFLAGS_COARRAY "-coarray"
    CACHE STRING "Fortuno: Flags needed for coarray features when compling"
  )
  set(
    FORTUNO_LDFLAGS_COARRAY "-coarray"
    CACHE STRING "Fortuno: Flags needed for coarray features when linking"
  )
  option(FORTUNO_WITH_FPP "Fortuno: whether compiler supports fpp macros" ON)
  set(
    FORTUNO_FFLAGS_FPP ""
    CACHE STRING "Fortuno: Flags needed to process source with fpp macros"
  )

elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "NAG")

  # Default settings for the NAG compiler

  set(
    FORTUNO_FFLAGS_THREADSAFE "-thread_safe"
    CACHE STRING "Fortuno: Flags needed to enforce thread-safe build during compilation"
  )
  set(
    FORTUNO_LDFLAGS_THREADSAFE "-thread_safe"
    CACHE STRING  "Fortuno: Flags needed to enforce thread-safe build during linking"
  )
  set(
    FORTUNO_FFLAGS_COARRAY "-coarray"
    CACHE STRING "Fortuno: Flags needed for coarray features when compling"
  )
  set(
    FORTUNO_LDFLAGS_COARRAY "-coarray"
    CACHE STRING "Fortuno: Flags needed for coarray features when linking"
  )
  option(FORTUNO_WITH_FPP "Fortuno: whether compiler supports fpp macros" ON)
  set(
    FORTUNO_FFLAGS_FPP ""
    CACHE STRING "Fortuno: Flags needed to process source with fpp macros"
  )

elseif (CMAKE_Fortran_COMPILER_ID STREQUAL "GNU")

  # Default settings for the GNU compiler

  set(
    FORTUNO_FFLAGS_THREADSAFE ""
    CACHE STRING "Fortuno: Flags needed to enforce thread-safe build during compilation"
  )
  set(
    FORTUNO_LDFLAGS_THREADSAFE ""
    CACHE STRING  "Fortuno: Flags needed to enforce thread-safe build during linking"
  )
  set(
    FORTUNO_FFLAGS_COARRAY "-fcoarray=single"
    CACHE STRING "Fortuno: Flags needed for coarray features when compling"
  )
  set(
    FORTUNO_LDFLAGS_COARRAY "-fcoarray=single"
    CACHE STRING "Fortuno: Flags needed for coarray features when linking"
  )
  option(FORTUNO_WITH_FPP "Fortuno: whether compiler supports fpp macros" ON)
  set(
    FORTUNO_FFLAGS_FPP "-ffree-line-length-none"
    CACHE STRING "Fortuno: Flags needed to process source with fpp macros"
  )

else ()

  # Dummy settings (you might have to override them)

  set(
    FORTUNO_FFLAGS_THREADSAFE ""
    CACHE STRING "Fortuno: Flags needed to enforce thread-safe build during compilation"
  )
  set(
    FORTUNO_LDFLAGS_THREADSAFE ""
    CACHE STRING  "Fortuno: Flags needed to enforce thread-safe build during linking"
  )
  set(
    FORTUNO_FFLAGS_COARRAY ""
    CACHE STRING "Fortuno: Flags needed for coarray features when compling"
  )
  set(
    FORTUNO_LDFLAGS_COARRAY ""
    CACHE STRING "Fortuno: Flags needed for coarray features when linking"
  )
  option(FORTUNO_WITH_FPP "Fortuno: whether compiler supports fpp macros" OFF)
  set(
    FORTUNO_FFLAGS_FPP ""
    CACHE STRING "Fortuno: Flags needed to process source with fpp macros"
  )

endif ()
