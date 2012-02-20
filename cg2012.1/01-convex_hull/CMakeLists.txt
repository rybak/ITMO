cmake_minimum_required(VERSION 2.6)

project(convexhull-checker)
set(sources checker.cpp)
add_definitions(-O2 -frounding-math)
message("flags=${CMAKE_CXX_FLAGS_RELEASE}")
link_libraries(CGAL gmp)
add_executable(convexhull-checker ${sources})
