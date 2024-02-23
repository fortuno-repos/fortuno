cmake_minimum_required(VERSION 3.22...3.28)

project(
  Fortuno_Export
  VERSION 0.1.0
  DESCRIPTION "Testing the CMake build info exported by Fortuno"
  LANGUAGES Fortran
)

find_package(Fortuno REQUIRED)
message(STATUS "Fortuno unit testing system: ${Fortuno_VERSION}")

add_executable(testapp testapp.f90)
target_link_libraries(testapp PRIVATE Fortuno::Fortuno)
