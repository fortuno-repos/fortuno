*********************************************************
Fortuno – flextensible unit testing framework for Fortran
*********************************************************

The **Fortuno** (Fortran Unit Testing Objects) project offers a flexible &
extensible, object oriented unit testing framework for the Fortran language. It
puts strong emphasis on the simplicity of the user interface by minimizing the
amount of boiler plate code when writing unit tests, as well as to modularity
and extensibility by offering building blocks for customized unit testing
systems.

This is an automatically deployed version of the `Fortuno repository
<https://github.com/fortuno-repos/fortuno>`_ for projects building with the
`Fortran package manager (fpm) <https://fpm.fortran-lang.org/>`_. If you are
using `fpm <https://fpm.fortran-lang.org/>`_ in your project, add one (and only
one) of the following repositories as development dependency to your
``fpm.toml`` manifest file in order to use the Fortuno unit testing framework:

* `<https://github.com/fortuno-repos/fortuno-fpm-serial.git>`_: if you only need
  the **serial interface** offered by the ``fortuno_serial`` module::

    [dev-dependencies]
    fortuno = { git = "https://github.com/fortuno-repos/fortuno-fpm-serial.git" }


* `<https://github.com/fortuno-repos/fortuno-fpm-mpi.git>`_: if you also need
  (additional to the serial one) the **MPI interface** offered by the
  ``fortuno_mpi`` module. Your project must declare the MPI framework as an
  additional meta-package dependency::

      [dependencies]
      mpi = "*"

      [dev-dependencies]
      fortuno = { git = "https://github.com/fortuno-repos/fortuno-fpm-mpi.git" }


* `<https://github.com/fortuno-repos/fortuno-fpm-coarray.git>`_: if you also
  need (additional to the serial one) the **coarray interface** offered by the
  ``fortuno_coarray`` module. Your project must be compiled with the right
  compiler and linker flags enabling coarray features::

      [dev-dependencies]
      fortuno = { git = "https://github.com/fortuno-repos/fortuno-fpm-coarray.git" }


If your projects uses the `CMake <https://cmake.org/>`_ or the `Meson
<https://mesonbuild.com/>`_ build systems, you must use the `Fortuno repository
<https://github.com/fortuno-repos/fortuno>`_ directly.

For documentation, pull requests, issues, etc. refer to the `Fortuno repository
<https://github.com/fortuno-repos/fortuno>`_.


License
=======

Fortuno is licensed under the `BSD-2-Clause Plus Patent License <LICENSE>`_.
This `OSI-approved <https://opensource.org/licenses/BSDplusPatent>`_ license
combines the 2-clause BSD license with an explicit patent grant from
contributors. The SPDX license identifier for this project is
`BSD-2-Clause-Patent <https://spdx.org/licenses/BSD-2-Clause-Patent.html>`_.
