*********************************************************
Fortuno – flextensible unit testing framework for Fortran
*********************************************************

**Fortuno** (Fortran Unit Testing Objects) is a flexible & extensible,
object-oriented unit testing framework designed for the Fortran programming
language. It emphasizes ease of use by minimizing boiler plate code when writing
tests while prioratizing modularity and extensibility. Fortuno provides the
essential building blocks to help developers create customized unit testing
solutions.

Fortuno provides:

- serial unit testing,

- parallel unit testing for MPI- and coarray-parallel projects,

- simple unit tests,

- fixtured tests,

- parametrized tests,

- automatic test registration (in combination with the `Fypp-preprocessor
  <https://github.com/aradi/fypp>`_), and

- integration with the `fpm <https://fpm.fortran-lang.org/>`_, `CMake
  <https://cmake.org/>`_ and `Meson <https://mesonbuild.com/>`_ build systems.

**Documentation** can be found on the `Fortuno documentation site
<https://fortuno.readthedocs.io>`_. Additionally, you may want to explore the
examples provided in the `example <example/>`_ folder.


Obtaining Fortuno
=================

You can obtain Fortuno by installing it as a Conda package, downloading it and
building it during your build process, or manually building and installing it
from source.


Install Fortuno as a Conda package
----------------------------------

The easiest way to obtain Fortuno is by installing a precompiled version via the
Conda package management framework from the `conda-forge
<https://conda-forge.org/>`_ channel. Ensure you are using either a Conda client
with this channel as default (e.g. `miniforge
<https://github.com/conda-forge/miniforge>`_) or manually add the channel. If
you're not familiar with Conda, consult the `miniforge
<https://github.com/conda-forge/miniforge>`_ project for setup instructions.

After switching to the desired Conda environment, run ::

  conda install 'fortuno=*=nompi_*'

to obtain Fortuno with serial interface only. If you wish to use Fortuno for
testing both serial and MPI-parallel routines, run ::

  conda install 'fortuno=*=openmpi_*'

or ::

  conda install 'fortuno=*=mpich_*'

depending on the preferred MPI-framework.

*Note:* The coarray interface is not available through Conda yet. To use the
coarray interface, build Fortuno during your build process or manually, as
described below.


Building Fortuno as part of your build process
----------------------------------------------

If your project is built with Fpm, CMake or Meson, you can seamlessly download
and build Fortuno as part of your project's build process. The setup steps differ
depending on the build system you are using:

* **Fpm**: Add Fortuno as a development dependency in your ``fpm.toml`` file:

  * **Serial interface**::

      [dev-dependencies]
      fortuno = { git = "https://github.com/fortuno-repos/fortuno-fpm-serial.git" }

  * **MPI interface**::

      [dev-dependencies]
      fortuno = { git = "https://github.com/fortuno-repos/fortuno-fpm-mpi.git" }

  * **Coarray interface**::

      [dev-dependencies]
      fortuno = { git = "https://github.com/fortuno-repos/fortuno-fpm-coarray.git" }


* **CMake**: Add the relevant snippet to your ``CMakeLists.txt`` file:

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

    *Note*: If Fortuno is already installed on your system, these settings will
    automatically use the installed version instead of downloading and building
    it during your build process.

* **Meson**: Create a ``fortuno.wrap`` file in the ``subprojects/`` directory
  with the following content::

    [wrap-git]
    directory=fortuno
    url=https://github.com/fortuno-repos/fortuno
    revision=main

  Then register Fortuno as a subproject in ``meson.build``:

  * **Serial interface**::

      fortuno_serial_dep = dependency(
        'fortuno_serial',
        fallback: ['fortuno', 'fortuno_serial_dep']
      )

  * **MPI interface**::

      fortuno_mpi_dep = dependency(
        'fortuno_mpi',
        fallback: ['fortuno', 'fortuno_mpi_dep'],
        default_options: {'with_mpi': true}
      )

  * **Coarray interface**::

      fortuno_coarray_dep = dependency(
        'fortuno_coarray',
        fallback: ['fortuno', 'fortuno_coarray_dep'],
        default_options: {
          'with_coarray': true,
          'fflags_coarray': fflags_coarray,
          'ldflags_coarray': ldflags_coarray,
        },
      )

    Define ``fflags_coarray`` and ``ldflags_coarray`` in your project to contain
    the flags needed to compile and link coarray-parallel code.

    *Note*: If Fortuno is already installed on your system, these settings will
    automatically use the installed version instead of downloading and building
    it during your build process.


Building and installing Fortuno from source
-------------------------------------------

You can also build and install Fortuno from source using a typical CMake
workflow.

* Review the ``config.cmake`` file for customizable build variables.

* Configure Fortuno::

    mkdir build
    FC=gfortran cmake -DCMAKE_INSTALL_PREFIX=${HOME}/opt/fortuno -B build

  Ensure CMake selects the correct Fortran compiler by explicitly setting the
  ``FC`` environment variable. You should also customize the installation
  directory by setting ``CMAKE_INSTALL_PREFIX``.

* Build the library::

    cmake --build build

* Install Fortuno::

    cmake --install build


Using the installed library
...........................

To integrate the manually installed Fortuno library into your project, follow
these instructions based on your build system:

* **CMake**:  Follow the CMake instructions outlined earlier. Ensure the
  ``CMAKE_PREFIX_PATH`` environment variable includes Fortuno's installation
  location::

    export CMAKE_PREFIX_PATH="${HOME}/opt/fortuno:${CMAKE_PREFIX_PATH}"

* **Meson**: Follow the Meson instructions from the previous section. Set the
  ``PKG_CONFIG_PATH`` environment variable to include Fortuno’s installation
  location::

    export PKG_CONFIG_PATH="${HOME}/opt/fortuno/lib/pkgconfig:${PKG_CONFIG_PATH}"

  (Depending on your Linux distribution, you might need to use ``lib64`` instead
  of ``lib``.)

* **Other build systems (e.g., Make)**: Follow your usual workflow. Add the
  directory containing the installed ``.mod`` files to the compiler's search
  path using the appropriate compiler flag::

    -I${HOME}/opt/fortuno/lib/modules

  Link the appropriate interface-specific library and the general library with
  the appropriate compiler/linker flags::

    -L${HOME}/opt/fortuno/lib -lfortuno_serial -lfortuno

  (You may need to use ``lib64`` instead of ``lib`` depending on your system's
  configuration.)


Quickstart
==========

The easiest way to start a new project utilizing the Fortuno unit testing
framework is by using the `Cookiecutter-Fortran-project
<https://github.com/fortuno-repos/cookiecutter-fortran-project>`_ template. This
tool creates a minimal project ready for building, testing, and installation,
with options to select your preferred build system (CMake, Fpm, or Meson),
parallelization method (serial, MPI-parallel, or coarray-parallel), and Fortuno
integration.

To add Fortuno unit tests to an existing project, follow the instructions below.
The examples assume your library includes a module called ``mylib`` with a
``factorial()`` function for calculating the factorial of integers. Adjust the
names to match your actual library and function names.


Writing unit tests
------------------

Writing unit tests in Fortuno is straightforward. For basic cases, tests are
written as simple subroutines without arguments. Minimal additional code is
required to register the tests and provide a command-line test driver.

For example, the following minimal working example tests a ``factorial()``
function in a hypothetical library ``mylib``::

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

* **Fpm**: If the ``testapp.f90`` source file is stored in the ``test/`` folder,
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
  dependency. Also, include your library (e.g., ``mylib_dep``) as a dependency::

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

* **Fpm**::

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
explore the examples in the `example <example/>`_ folder.


Compiler compatibility
======================

To provide a simple interface with maximum flexibility and extensibility,
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

Fortuno is licensed under the `BSD-2-Clause Plus Patent License <LICENSE.txt>`_.
This `OSI-approved <https://opensource.org/licenses/BSDplusPatent>`_ license
combines the 2-clause BSD license with an explicit patent grant from
contributors. The SPDX license identifier for this project is
`BSD-2-Clause-Patent <https://spdx.org/licenses/BSD-2-Clause-Patent.html>`_.
