*********************************************************
Fortuno – flextensible unit testing framework for Fortran
*********************************************************

**Fortuno** (Fortran Unit Testing Objects) is a flexible & extensible,
object-oriented unit testing framework designed for the Fortran programming
language. It emphasizes ease of use by minimizing boiler plate code when writing
tests, while also prioratizing modularity and extensibility. Fortuno provides
the essential building blocks to help developers create customized unit testing
solutions.

Fortuno provides:

- simple unit tests,

- fixtured tests,

- parametrized tests,

- serial unit testing,

- parallel unit testing for MPI- and coarray-parallel projects, and

- integration with the `fpm <https://fpm.fortran-lang.org/>`_, `CMake
  <https://cmake.org/>`_ and `Meson <https://mesonbuild.com/>`_ build systems.

**Documentation** can be found on the `Fortuno documentation site
<https://fortuno.readthedocs.io>`_. Additionally, you may want to explore the
examples provided in the `example folder <example/>`_.


Quickstart
==========

The easiest way to begin a new project with the Fortuno unit testing framework
is by using the `Cookiecutter-Fortran-project
<https://github.com/fortuno-repos/cookiecutter-fortran-project>`_ template
generator. This tool creates a minimal project setup that’s ready for building,
testing, and installation, with options to select your preferred build system
(CMake, Fpm, or Meson), parallelization method (serial, MPI-parallel, or
coarray-parallel), and built-in Fortuno integration.

If you'd like to add Fortuno unit tests to an existing project, follow the
instructions below. In the examples, it’s assumed your library includes a module
called ``mylib`` that provides a ``factorial()`` function for calculating the
factorial of integers. You can adjust these names to match your actual library
and function names.


Obtaining Fortuno during your build process
-------------------------------------------

The simplest way to integrate Fortuno into your project is by downloading and
building it as part of your project's build process. The steps vary depending on
the build system you're using:

* **Fpm**: Add Fortuno as a development dependency by including the following
  lines in your fpm.toml file:

  * **Serial interface**::

      [dev-dependencies]
      fortuno = { git = "https://github.com/fortuno-repos/fortuno-fpm-serial.git" }

  * **MPI interface**::

      [dev-dependencies]
      fortuno = { git = "https://github.com/fortuno-repos/fortuno-fpm-mpi.git" }

  * **Coarray interface**::

      [dev-dependencies]
      fortuno = { git = "https://github.com/fortuno-repos/fortuno-fpm-coarray.git" }


* **CMake**: Add the relevant snippet to your project's ``CMakeLists.txt``
  file:

  * **Serial interface**::

      include(FetchContent)
      FetchContent_Declare(
        Fortuno
        GIT_REPOSITORY "https://github.com/fortuno-repos/fortuno"
        GIT_TAG "main"
      )
      FetchContent_MakeAvailable(Fortuno)

  * **MPI interface**::

      option(FORTUNO_WITH_MPI "Fortuno: whether to build the MPI interface" ON)
      include(FetchContent)
      FetchContent_Declare(
        Fortuno
        GIT_REPOSITORY "https://github.com/fortuno-repos/fortuno"
        GIT_TAG "main"
      )
      FetchContent_MakeAvailable(Fortuno)

  * **Coarray interface**::

      option(FORTUNO_WITH_COARRAY "Fortuno: whether to build the coarray interface" ON)
      include(FetchContent)
      FetchContent_Declare(
        Fortuno
        GIT_REPOSITORY "https://github.com/fortuno-repos/fortuno"
        GIT_TAG "main"
      )
      FetchContent_MakeAvailable(Fortuno)

    Additionally, you may want to define the cache variables
    ``FORTUNO_FFLAGS_COARRAY`` and ``FORTUNO_LDFLAGS_COARRAY`` with the
    appropriate compiler and linker flags for coarray parallelism.

    *Note*: If Fortuno is already installed on your system, the settings
    described above will automatically use the installed version rather than
    downloading and building it during your project's build process.

* **Meson**: Create a ``fortuno.wrap`` file in the subprojects/ directory
  (create it if it doesn’t already exist) with the following content::

    [wrap-git]
    directory=fortuno
    url=https://github.com/fortuno-repos/fortuno
    revision=main

  Register Fortuno as a subproject by adding the following to your main
  ``meson.build`` file:

  * **Serial interface**::

      fortuno_serial_dep = dependency(
        'fortuno-serial',
        fallback: ['fortuno', 'fortuno_serial_dep']
      )

  * **MPI interface**::

      fortuno_mpi_dep = dependency(
        'fortuno-mpi',
        fallback: ['fortuno', 'fortuno_mpi_dep'],
        default_options: {'with_mpi': true}
      )

  * **Coarray interface**::

      fortuno_coarray_dep = dependency(
        'fortuno-coarray',
        fallback: ['fortuno', 'fortuno_coarray_dep'],
        default_options: {
          'with_coarray': true,
          'fflags_coarray': fflags_coarray,
          'ldflags_coarray': ldflags_coarray,
        },
      )

    The variables ``fflags_coarray`` and ``ldflags_coarray`` should be defined
    in your project to contain the flags required to compile and link
    coarray-parallel code.

    *Note*: If Fortuno is already installed on your system, the settings
    described above will automatically use the installed version rather than
    downloading and building it during your project's build process.


Installing Fortuno on your system
---------------------------------

As an alternative to downloading and building Fortuno on-the-fly during your
project's build process, it is also possible to install the library directly on
your system and use the installed version during the build. This can be useful
for avoiding repeated downloads as well as for using Fortuno with other build
systems (e.g. Make).

To install Fortuno, follow the standard CMake workflow:

* Review the ``config.cmake`` file for variables that allow you to customize the
  build.

* Configure Fortuno::

    mkdir build
    FC=gfortran cmake -DCMAKE_INSTALL_PREFIX=~/opt/fortuno -B build

  Ensure CMake selects the correct Fortran compiler by explicitly setting the
  ``FC`` environment variable. You should also customize the installation
  directory by setting the ``CMAKE_INSTALL_PREFIX`` variable accordingly.

* Build the library::

    cmake --build build

* Install Fortuno::

    cmake --install build

The installed library includes package configuration files for both CMake and
Meson. The settings to invoke Fortuno into CMake and Meson built projects are
the same as described in the previous section. Just make sure to adjust the
``CMAKE_PREFIX_PATH`` and ``PKG_CONFIG_PATH`` environment variables according to
Fortuno's installation location, so that the build system can locate the
installed library.


Writing unit tests
------------------

In Fortuno, writing unit tests is straightforward. For basic cases, tests are
written as simple subroutines without arguments. Aside from the test routines
themselves, only a minimal amount of additional code is required to register the
tests in the framework and provide a command-line test driver to execute them.

For example, given a hypothetical library ``mylib`` that provides a
``factorial()`` function, a minimal test program checking the results for two
different input values might look like this::

  ! file: testapp.f90

  !> Module containing the tests
  module testapp_tests
    use mylib, only : factorial
    use fortuno_serial, only : is_equal, test => serial_case_item, check => serial_check, test_list
    implicit none

  contains

    !> Returns the tests in this module
    function tests()
      type(test_list) :: tests

      tests = test_list([&
          test("factorial_0", test_factorial_0),&
          test("factorial_1", test_factorial_1)&
      ])

    end function tests

    ! Test: 0! = 1
    subroutine test_factorial_0()
      call check(factorial(0) == 1)
    end subroutine test_factorial_0

    ! Test: 1! = 1
    ! This routine uses is_equal() for comparison in order to obtain detailed
    ! information in case of a failure.
    subroutine test_factorial_1()
      call check(is_equal(factorial(1), 1))
    end subroutine test_factorial_1

  end module testapp_tests


  !> Test app driving Fortuno unit tests.
  program testapp
    use fortuno_serial, only : execute_serial_cmd_app
    use testapp_tests, only : tests
    implicit none

    ! Register tests by providing name and subroutine to run for each test.
    ! Note: this routine does not return but stops the program with the right exit code.
    call execute_serial_cmd_app(tests())

  end program testapp


Bulding the test-driver app
---------------------------

To run your unit tests, you'll first need to build the test driver app using
your chosen build system:

* **fpm**: If the ``testapp.f90`` source file is stored in the ``test/`` folder,
  fpm will automatically compile it and link it with the Fortuno library when
  you build your project. Simply run::

    fpm build

* **CMake**:  In your ``CMakeLists.txt`` file, declare an executable ``testapp``
  using ``testapp.f90`` as the source file and add ``Fortuno::fortuno_serial``
  as a dependency. Be sure to also link your library (e.g. ``mylib``).
  Additionally, register the executable as a test, so that it can be executed
  with ``ctest``::

    add_executable(testapp testapp.f90)
    target_link_libraries(testapp PRIVATE mylib Fortuno::fortuno_serial)
    add_test(NAME factorial COMMAND testapp)

  *Note*:  If you are using the MPI or coarray interface, replace
  ``Fortuno::fortuno_serial`` with ``Fortuno::fortuno_mpi`` or
  ``Fortuno::fortuno_coarray``, respectively.

  Ensure that you call ``enable_testing()`` in your main ``CMakeLists.txt`` file
  before defining the rules for ``testapp`` so that ``ctest`` can be used for
  testing.

  Afterward, configure and build your project as usual::

    cmake -B _build
    cmake --build _build

* **Meson**: In the ``meson.build`` file, declare an executable ``testapp``
  using ``testapp.f90`` as the source and ``fortuno_serial_dep`` as a
  dependency. Also include your library (e.g., ``mylib_dep``) as a dependency::

    testapp_exe = executable(
      'testapp',
      sources: ['testapp.f90'],
      dependencies: [mylib_dep, fortuno_serial_dep],
    )
    test('factorial', testapp_exe)

  *Note*: If you're using the MPI or coarray interface, replace
  ``fortuno_serial_dep`` with ``fortuno_mpi_dep`` or ``fortuno_coarray_dep``,
  respectively.

  Build your project as usual::

    meson setup _build
    ninja -C _build


Running the tests
-----------------

Once your test driver app is built, you can run the unit tests using the testing
features of your build system:

* **fpm**::

    fpm test

* **CMake**::

    ctest --verbose --test-dir _build

* **Meson**::

    meson test -v -C _build

The test results are conveyed through the exit code of the test app: zero
indicates success, while a non-zero value signals a failure. Additionally,
Fortuno logs detailed information to the console during the test run::

  === Fortuno - flextensible unit testing framework for Fortran ===

  # Executing test items
  ..

  # Test runs
  Total:      2
  Succeeded:  2  (100.0%)

  === Succeeded ===


Further information
--------------------

For more detailed explanations, additional features, and various use cases,
refer to the `Fortuno documentation <https://fortuno.readthedocs.io>`_ and
explore the examples in the `example folder <example/>`_.


Compiler compatibility
======================

To provide a simple interface along with maximum flexibility and extensibility,
Fortuno leverages modern Fortran constructs extensively. Therefore, building
Fortuno requires a compiler that supports Fortran 2018. Below is a table of
compilers that have been successfully tested for building Fortuno. We recommend
using these or newer versions.

+------------------------+-----------------------------------------------------+
| Compiler               | Status                                              |
+========================+=====================================================+
| Intel 2024.{0,1,2}     | * OK (serial, mpi, coarray)                         |
+------------------------+-----------------------------------------------------+
| NAG 7.2 (build 7202)   | * OK (serial, mpi, coarray)                         |
+------------------------+-----------------------------------------------------+
| GNU 13.2, 14.1         | * OK (serial, mpi)                                  |
|                        | * untested (coarray)                                |
+------------------------+-----------------------------------------------------+

If you know of other compilers that can successfully build Fortuno, please
consider opening a pull request to update this table.


License
=======

Fortuno is licensed under the `BSD-2-Clause Plus Patent License <LICENSE>`_.
This `OSI-approved <https://opensource.org/licenses/BSDplusPatent>`_ license
combines the 2-clause BSD license with an explicit patent grant from
contributors. The SPDX license identifier for this project is
`BSD-2-Clause-Patent <https://spdx.org/licenses/BSD-2-Clause-Patent.html>`_.
