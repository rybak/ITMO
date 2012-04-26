cmake_minimum_required(VERSION 2.6)

project(polylinesimplificationnotopologypreservation-generator)
set(sources correctness_generator.cpp perfomance_generator.cpp)
add_definitions(-O2 -frounding-math)
link_libraries(CGAL gmp)
add_executable(polylinesimplificationnotopologypreservation-generator ${sources})