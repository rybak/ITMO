cmake_minimum_required (VERSION 2.6)

project (segment_intersection)

set(CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake/Modules")

#set(CMAKE_BUILD_TYPE RelWithDebInfo)
set(CMAKE_BUILD_TYPE Release)

find_package(CGAL REQUIRED)

include(${CGAL_USE_FILE})

include_directories(${CGAL_INCLUDE_DIRS})
set (LIBS ${CGAL_LIBRARIES} ${CGAL_3RD_PARTY_LIBRARIES})

#message(STATUS ${LIBS})

add_executable (check check.cpp)
