project(registry CXX)
cmake_minimum_required(VERSION 3.25)

add_executable(openfast_registry_cpp
  src/registry.hpp
  src/registry.cpp
  src/record.cpp
  src/record.hpp
  src/node.hpp
  src/templates.hpp
  src/main.cpp
)

set_property(TARGET openfast_registry_cpp PROPERTY CXX_STANDARD 11)