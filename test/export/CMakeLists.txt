cmake_minimum_required(VERSION 3.22...3.28)

project(
  Fortuno_Test_Export
  VERSION 0.1.0
  DESCRIPTION "Testing the CMake build info exported by Fortuno"
  LANGUAGES Fortran
)

find_package(Fortuno REQUIRED)

add_executable(testapp)
add_subdirectory(app)
