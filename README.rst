*******************************************************
Fortuno – extensible unit testing framework for Fortran
*******************************************************

The **Fortuno** (Fortran Unit Testing Objects) project offers a flexible,
extensible, object oriented unit testing framework for the Fortran language. It
puts strong emphasis on the simplicity of the user interface by minimizing the
amount of boiler plate code when writing unit tests, as well as to modularity
and extensibility by offering building blocks for customized unit testing
systems.

**Fortuno** provides

- simple unit tests,

- fixtured tests,

- parametrized tests,

- serial unit testing,

- parallel unit testing for MPI- and coarray-parallel projects, and

- integration with the `fpm <https://fpm.fortran-lang.org/>`_, `CMake
  <https://cmake.org/>`_ and `Meson <https://mesonbuild.com/>`_ build systems.

Detailed **documentation** is available on the `Fortuno documentation
<https://fortuno.readthedocs.io>`_ page. You can also have a look at the
examples in the `example folder <example/>`_.

The development can be followed and joined at the `Fortuno project
<https://github.com/fortuno-repos/fortuno>`_  page on GitHub.


Quickstart
==========

The following instructions demonstrate how to add unit tests testing serial code
via Fortuno to an existing project, which uses fpm, CMake or Meson as build
system. If you are not familiar with any of these build systems, visit the
`Fortuno documentation <https://fortuno.readthedocs.io>`_ for a step-by-step
guide starting from scratch.

In the examples below, we will assume that your library has a module ``mylib``,
which provides a function ``factorial()`` for calculating the factorial of
integers. Adapt those names to your actual library and routine names.


Obtaining Fortuno
-----------------

The easiest way to obtain Fortuno is to download and build it as part of your
project's build process. The actual steps depend on your build system:

* **fpm:** Register Fortuno as a development dependency by adding the following
  lines to your ``fpm.toml`` file::

    [dev-dependencies]
    fortuno = { git = "https://github.com/fortuno-repos/fortuno" }

* **CMake:** Add the following snippet to the ``CMakeLists.txt`` file in the
  root folder of your project::

    include(FetchContent)
    FetchContent_Declare(
      Fortuno
      GIT_REPOSITORY "https://github.com/fortuno-repos/fortuno"
      GIT_TAG "main"
    )
    FetchContent_MakeAvailable(Fortuno)

* **Meson:** Create the file ``fortuno.wrap`` in the ``subprojects/`` folder
  of your project (create the folder, if it does not exist yet) with following
  content::

    [wrap-git]
    directory=fortuno
    url=https://github.com/fortuno-repos/fortuno
    revision=main

  Register Fortuno as a subproject by adding the following to your main
  ``meson.build`` file::

    fortuno_serial_dep = dependency('fortuno-serial', fallback: ['fortuno', 'fortuno_serial_dep'])


Writing unit tests
------------------

For the basic cases, Fortuno unit tests are plain subroutines without any
arguments. Apart of your test routines, you only need a minimal amount of code
to register them in the test framework and to provide access to them via a
command line test driver app.

Given the hypothetical library ``mylib`` providing the function ``factorial()``,
the minimal test program checking the results for two different input values
could look as follows::

  ! file: testapp.f90

  !> Test app driving Fortuno unit tests.
  program testapp
    use mylib, only : factorial
    use fortuno_serial, only : execute_serial_cmd_app, is_equal, test => serial_case_item,&
        & check => serial_check
    implicit none

    !> Register tests by providing name and subroutine to run for each test.
    !> Note: this routine does not return but stops the program with the right exit code.
    call execute_serial_cmd_app(&
        testitems=[&
            test("factorial_0", test_factorial_0),&
            test("factorial_1", test_factorial_1)&
        ]&
    )

  contains

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

  end program testapp


Bulding the test-driver app
---------------------------

In order to run the unit tests, you must build the test driver app with your
build system:

* **fpm:** If you stored the test-driver app source ``testapp.f90`` in the
  ``test/`` folder, fpm will automatically compile it and link it with the
  Fortuno library when you build your project with ::

    fpm build

* **CMake:** Declare an executable ``testapp`` with ``testapp.f90`` as source
  and target ``Fortuno::fortuno_serial`` as dependency in the ``CMakeLists.txt``
  file. Add also the target name of your library (e.g. ``mylib``) as dependency.
  Additionally, register the executable as a test, so that it can be executed
  via ``ctest``::

    add_executable(testapp testapp.f90)
    target_link_libraries(testapp PRIVATE mylib Fortuno::fortuno_serial)
    add_test(NAME factorial COMMAND testapp)

  Make also sure to call ``enable_testing()`` in your main ``CMakeLists.txt``
  file before the rules for ``testapp`` are processed, so that you can use
  ``ctest`` for the testing.

  Now configure and build your project as usual::

    cmake -B _build
    cmake --build _build

* **Meson:** Declare an executable ``testapp`` with ``testapp.f90`` as source
  and ``fortuno_dep`` as dependency in the ``meson.build`` file. Add also your
  library (e.g. ``mylib_dep``) as dependency::

    testapp_exe = executable(
      'testapp',
      sources: ['testapp.f90'],
      dependencies: [mylib_dep, fortuno_serial_dep],
    )
    test('factorial', testapp_exe)

  Build your project as usual::

    meson setup _build
    ninja -C _build


Running the tests
-----------------

You run the units tests by executing the test app via the testing feature of
your build system:

* **fpm:** ::

    fpm test

* **CMake:** ::

    ctest --verbose --test-dir _build

* **Meson:** ::

    meson test -v -C _build

The result is communicated via the testapp's exit code to the build framework
(zero for success, and non-zero for failure). Additionally, Fortuno logs details
to the console::

  === Fortuno - extensible unit testing framework for Fortran ===

  # Executing test items
  ..

  # Test runs
  Total:      2
  Succeeded:  2  (100.0%)

  === Succeeded ===


Further information
--------------------

Check out the `Fortuno documentation <https://fortuno.readthedocs.io>`_ for more
detailed explanations, further features and use cases.


Compiler compatibility
======================

In order to offer a simple user interface and to allow for maximal reusability
and extensibility, Fortuno uses modern Fortran constructs extensively. Building
Fortuno requires a compiler with Fortran 2018 support. The following table gives
an overview over the compilers which were successfully tested for building
Fortuno. We recommend to use those compilers or any newer versions of them.

+------------------------+-----------------------------------------------------+
| Compiler               | Status                                              |
+========================+=====================================================+
| Intel 2024.0           | * serial: OK                                        |
|                        | * mpi: OK                                           |
|                        | * coarray: OK                                       |
+------------------------+-----------------------------------------------------+
| NAG 7.2 (build 7202)   | * serial: OK                                        |
|                        | * mpi: OK                                           |
|                        | * coarray: OK                                       |
+------------------------+-----------------------------------------------------+
| GNU 13.2               | * serial: OK                                        |
|                        | * mpi: OK                                           |
|                        | * coarray: not tested yet                           |
+------------------------+-----------------------------------------------------+

If you are aware of any other compilers being able to build Fortuno, open a pull
request to update the table.


License
=======

Fortuno is licensed under the `BSD-2-Clause Plus Patent License <LICENSE>`_.
This `OSI-approved <https://opensource.org/licenses/BSDplusPatent>`_ license
combines the 2-clause BSD license with an explicit patent grant from
contributors. The SPDX license identifier for this project is
`BSD-2-Clause-Patent <https://spdx.org/licenses/BSD-2-Clause-Patent.html>`_.
