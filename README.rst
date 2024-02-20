*******************************************************
Fortuno – extensible unit testing framework for Fortran
*******************************************************

The **Fortuno** (Fortran Unit Testing Objects) project offers a flexible,
modern, object oriented unit testing framework for the Fortran language. It puts
strong emphasis on the simplicity of the user interface (minimizing the amount
of boiler plate code when writing unit tests) as well as to modularity and
extensibility (offering building blocks for customized unit testing systems.)

It is written in Fortran 2018 and can be directly used without a need for a
special pre-processor.

**Fortuno** provides

- simple unit tests,

- fixtured tests,

- parameterized tests,

- serial unit testing,

- parallel unit testing for MPI- and coarray-parallel projects, and

- seamless integration with the CMake and Fpm build systems.

Detailed **documentation** is available on the `Fortuno documentation page
<https://fortuno.readthedocs.io>`_. You can also have a look at some of the
example files in the `example folder <example/>`_.

The development can be followed on the `Fortuno project page
<https://github.com/fortuno-repos/fortuno>`_  on GitHub.


Known issues
============

In order to be flexible, extensible and simple, Fortuno uses modern Fortran
constructs extensively. Unfortunately, this seems to be still challenging for
some compilers. The following table gives an overview over the compilers which
were tested for building Fortuno.

+------------------------+-----------------------------------------------------+
| Compiler               | Status                                              |
+========================+=====================================================+
| Intel 2024.0           | * serial: OK                                        |
| x86_64/Linux           | * mpi (with Intel MPI): OK                          |
+------------------------+-----------------------------------------------------+
| NAG 7.1 (build 7145)   | * serial: OK                                        |
| x86_64/Linux           | * mpi (MPICH 4.1): OK                               |
+------------------------+-----------------------------------------------------+
| GNU 13.2               | * serial: Failed (Internal compiler error)          |
| x86_64/Linux           |                                                     |
+------------------------+-----------------------------------------------------+

If you are aware of other compilers being able to build Fortuno, open an issue
or a pull request, so that we can update the table accordingly.


License
=======

Fortuno is licensed under the `BSD-2-Clause Plus Patent License
<https://opensource.org/licenses/BSDplusPatent>`_. The SPDX license identifier
for this project is `BSD-2-Clause-Patent
<https://spdx.org/licenses/BSD-2-Clause-Patent.html>`_.
