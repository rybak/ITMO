cmake_minimum_required(VERSION 2.6)

project(orientation-testgen)

set(sources testgen.cpp)
add_definitions(-O2 -frounding-math -std=c++0x)
add_executable(orientation-testgen ${sources})
