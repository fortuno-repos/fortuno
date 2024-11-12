****************
Fortuno examples
****************

Documented examples demonstrating the usage of the Fortuno unit testing
framework for various test scenarios.

**Serial unit tests**

- `serial <serial>`_: Unit tests in pure Fortran.

- `serial-fpp <serial-fpp>`_: Unit tests utilizing fpp-style macros (which are
  natively understood by basically all Fortran compilers). Allows for
  automatically added file and line information when reporting failure.

- `serial-fypp <serial-fypp>`_: Unit tests utilizing Fypp macros (helpful if
  your project uses the Fypp-preprocessor). Allows for automatic test
  registration as well as for automatic file and line information when reporting
  failure.


**MPI-parallel unit tests**

- `mpi <mpi>`_: Unit tests in pure Fortran.

- `mpi-fpp <mpi-fpp>`_: Unit tests utilizing fpp-style macros (which are
  natively understood by basically all Fortran compilers). Allows for
  automatically added file and line information when reporting failure.

- `mpi-fypp <mpi-fypp>`_: Unit tests utilizing Fypp macros (helpful if your
  project uses the Fypp-preprocessor). Allows for automatic test registration as
  well as for automatic file and line information when reporting failure.


**Coarray-parallel unit tests**

- `coarray <coarray>`_: Unit tests in pure Fortran.

- `coarray-fpp <coarray-fpp>`_: Unit tests utilizing fpp-style macros (which are
  natively understood by basically all Fortran compilers). Allows for
  automatically added file and line information when reporting failure.

- `coarray-fypp <coarray-fypp>`_: Unit tests utilizing Fypp macros (helpful if
  your project uses the Fypp-preprocessor). Allows for automatic test
  registration as well as for automatic file and line information when reporting
  failure.
