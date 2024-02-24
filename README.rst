*******************************************************
Fortuno – extensible unit testing framework for Fortran
*******************************************************

The **Fortuno** (Fortran Unit Testing Objects) project offers a flexible,
extensible, object oriented unit testing framework for the Fortran language. It
puts strong emphasis on the simplicity of the user interface (minimizing the
amount of boiler plate code when writing unit tests) as well as to modularity
and extensibility (offering building blocks for customized unit testing
systems.)

It is written in Fortran 2018 and can be directly used without a need for a
special pre-processor.

**Fortuno** provides

- simple unit tests,

- fixtured tests,

- parametrized tests,

- serial unit testing,

- parallel unit testing for MPI- and coarray-parallel projects, and

- seamless integration with the `fpm <https://fpm.fortran-lang.org/>`_ and
  `CMake <https://cmake.org/>`_ build systems.

Detailed **documentation** is available on the `Fortuno documentation
<https://fortuno.readthedocs.io>`_ page. You can also have a look at the
examples in the `example folder <example/>`_.

The development can be followed and joined at the `Fortuno project
<https://github.com/fortuno-repos/fortuno>`_  page on GitHub.


Quickstart
==========

Obtaining Fortuno
-----------------

The easiest way to obtain Fortuno is to download and to build it as part of your
project's build process. The actual steps depend on your build system.

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


Writing unit tests
------------------

For the basic cases, Fortuno unit tests are plain subroutines without any
arguments. Additional to the unit test subroutines, you only need a minimal
amount of code to register the tests in the test framework and to provide access
to them via a command line test driver app.

Given a hypothetical library ``mylib`` providing a function ``factorial()`` for
calculating the factorial of integers, the minimal test program checking the
results for two different input values could look as follows::

  ! testapp.f90

  !> Test app driving Fortuno unit tests.
  program testapp
    use mylib, only : factorial
    use fortuno, only : execute_serial_cmd_app, is_equal, test => serial_case_item,&
        check => serial_check
    implicit none

    !> Register tests by providing name and subroutine to run for each test.
    !> Note: this routine does not return but stops the program with the right exit code.
    call execute_serial_cmd_app(&
        testitems=[&
            test("factorial_0", test_factorial_0),&
            test("factorial_1", test_factorial_1),&
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

In order to run the unit tests, the test driver app must be built with your
build system:

* **fpm:** If you stored the test-driver app source (``testapp.f90``) in the
  ``test/`` folder, fpm will automatically compile it and link it with the
  Fortuno library when you build your project::

    fpm build

* **CMake:** Register ``testapp.f90`` for compilation and add the
  ``Fortuno::Fortuno`` target as dependency in the relevant ``CMakeLists.txt``
  file. You would, of course, also have to specify your library (e.g. ``mylib``)
  as dependency. Additionally, register the executable as a test, so that it can
  be executed via ``ctest``::

    add_executable(testapp testapp.f90)
    target_link_libraries(testapp PRIVATE mylib Fortuno::Fortuno)
    add_test(NAME factorial COMMAND testapp)

  Make also sure to call ``enable_testing()`` in your main ``CMakeLists.txt``
  file before the rules for ``testapp`` are processed, so that you can use
  ``ctest`` for the testing.

  Now configure and build your project as usual::

    cmake -B _build
    cmake --build _build


Running the tests
-----------------

You run the units tests by executing the test app:

* **fpm:** Issue ::

    fpm test

* **CMake:** Run the CTest application::

    ctest --verbose --test-dir _build

  You might omit the ``--verbose`` option to suppress the detailed console
  output of Fortuno. You will only see the final result of the testing procedure
  then.

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


The behavior of the test app can be influenced using command line options. In
order to get a list of the registered tests, run the ``testapp`` executable with
the `-l` option::

  fpm test testapp -- -l   # with fpm

  _build/test/testapp -l   # with CMake assuming testapp.f90 is in the test/ subfolder

With the test app as defined above, you should obtain ::

  factorial_0
  factorial_1

It is also possible to select (or deselect) the tests to run by passing their
names as command line arguments (prefixed by ``~`` for deselection)::

  # Run only the test 'factorial_0'
  fpm test testapp -- factorial_0

  # Run all tests except 'factorial_0'
  fpm test testapp -- ~factorial_0


Further information
--------------------

This quickstart contains only as much information as strictly necessary for
starting with Fortuno. Please check out the `Fortuno documentation
<https://fortuno.readthedocs.io>`_ for further use cases, examples and more
detailed explanations.


Known issues
============

In order to offer a simple user interface, and to be modular, flexible and
extensible, Fortuno uses modern Fortran constructs extensively. Unfortunately,
this seems to be still challenging for some Fortran compilers. The following
table gives an overview over the compilers which were tested for building
Fortuno.

+------------------------+-----------------------------------------------------+
| Compiler               | Status                                              |
+========================+=====================================================+
| Intel 2024.0           | * serial: OK                                        |
| x86_64/Linux           | * mpi (with Intel MPI): OK                          |
+------------------------+-----------------------------------------------------+
| NAG 7.1 (build 7145)   | * serial: OK                                        |
| x86_64/Linux           | * mpi (MPICH 4.1): OK                               |
+------------------------+-----------------------------------------------------+
| GNU 13.2               | * serial: Fails (Internal compiler error)           |
| x86_64/Linux           |                                                     |
+------------------------+-----------------------------------------------------+

If you are aware of other compilers being able to build Fortuno, open a pull
request, so that we can update the table accordingly.


License
=======

Fortuno is licensed under the `BSD-2-Clause Plus Patent License <LICENSE>`_.
(This `OSI-approved <https://opensource.org/licenses/BSDplusPatent>`_ license
combines the 2-clause BSD license with an explicit patent grant from
contributors.) The SPDX license identifier for this project is
`BSD-2-Clause-Patent <https://spdx.org/licenses/BSD-2-Clause-Patent.html>`_.
