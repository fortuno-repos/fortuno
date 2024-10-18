*********************************************************
Fortuno – flextensible unit testing framework for Fortran
*********************************************************

  This repository is automatically deployed from the `Fortuno repository
  <https://github.com/fortuno-repos/fortuno>`_. Please direct any pull requests,
  issues, or other contributions to the primary repository.

**Fortuno** (Fortran Unit Testing Objects) is a flexible & extensible,
object-oriented unit testing framework designed for the Fortran programming
language. It emphasizes ease of use by minimizing boiler plate code when writing
tests, while also prioratizing modularity and extensibility. Fortuno provides
the essential building blocks to help developers create customized unit testing
solutions.

This repository is an automatically deployed version for projects built using
the `Fortran package manager (fpm) <https://fpm.fortran-lang.org/>`_. For
projects built with other systems like `CMake <https://cmake.org/>`_ or `Meson
<https://mesonbuild.com/>`_, you should refer to the `primary Fortuno repository
<https://github.com/fortuno-repos/fortuno>`_.

If your project uses fpm, simply add one (and only one) of the following
repositories as a development dependency in your ``fpm.toml`` file to integrate
Fortuno into your unit testing process:

* **Serial interface**: If your project only requires the serial interface from
  the ``fortuno_serial`` module, add this to your ``fpm.toml``::

    [dev-dependencies]
    fortuno = { git = "https://github.com/fortuno-repos/fortuno-fpm-serial.git" }


* **MPI interface**: If your project requires the MPI interface from the
  ``fortuno_mpi`` module, add the snippet below to your ``fpm.toml``. You also
  must declare the metapackage MPI as dependency::

      [dependencies]
      mpi = "*"

      [dev-dependencies]
      fortuno = { git = "https://github.com/fortuno-repos/fortuno-fpm-mpi.git" }


* **Coarray interface**: If your project needs the coarray interface offered by
  the ``fortuno_coarray`` module, use the following dependency. Ensure your
  project is compiled with the appropriate compiler and linker flags to enable
  coarray support::

      [dev-dependencies]
      fortuno = { git = "https://github.com/fortuno-repos/fortuno-fpm-coarray.git" }


For more information, including documentation, issues and pull requests, please
visit the `Fortuno repository <https://github.com/fortuno-repos/fortuno>`_.


License
=======

Fortuno is licensed under the `BSD-2-Clause Plus Patent License <LICENSE>`_.
This `OSI-approved <https://opensource.org/licenses/BSDplusPatent>`_ license
combines the 2-clause BSD license with an explicit patent grant from
contributors. The SPDX license identifier for this project is
`BSD-2-Clause-Patent <https://spdx.org/licenses/BSD-2-Clause-Patent.html>`_.
