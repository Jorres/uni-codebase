cmake_minimum_required(VERSION 3.8)
set(CMAKE_CXX_FLAGS "-pthread -fsanitize=address,undefined -Wall -std=c++14 -pedantic ${CMAKE_CXX_FLAGS}")
set(CMAKE_CXX_FLAGS_DEBUG "-fsanitize=address,undefined -D_GLIBCXX_DEBUG ${CMAKE_CXX_FLAGS_DEBUG}")
set(CMAKE_CXX_FLAGS_COVERAGE "-g --coverage")
set(LINK_FLAGS "-pthread ${LINK_FLAGS}")

include_directories(.)
add_subdirectory(gtest)

add_library(counted set.h iterators.h counted.h counted.cpp fault_injection.h fault_injection.cpp)

add_executable(std std.cpp)
target_link_libraries(std counted gtest)