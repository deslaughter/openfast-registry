project(registry CXX)
cmake_minimum_required(VERSION 3.25)
include(CTest) 

add_executable(openfast_registry_cpp
  src/main.cpp
  src/registry_gen_fortran.cpp
  src/registry_gen_c.cpp
  src/registry_parse.cpp
  src/registry.cpp
  src/registry.hpp
  src/templates.hpp
)

set_property(TARGET openfast_registry_cpp PROPERTY CXX_STANDARD 11)
add_subdirectory(test)