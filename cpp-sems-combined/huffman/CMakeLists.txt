cmake_minimum_required(VERSION 2.8)

project(huffman)

include_directories(${BIGINT_SOURCE_DIR})
                  
add_executable(huffman
               huffman.cpp
               lib_huffman.h
               lib_huffman.cpp
               reader.h
               writer.h
               exception.h
               huffman_test.cpp)

if(CMAKE_COMPILER_IS_GNUCC OR CMAKE_COMPILER_IS_GNUCXX)
  set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -Wall -std=c++11 -fsanitize=address,leak,undefined -pedantic")
  set(CMAKE_CXX_FLAGS_DEBUG "${CMAKE_CXX_FLAGS_DEBUG} -g -D_GLIBCXX_DEBUG")
  set(CMAKE_CXX_FLAGS_RELEASE "${CMAKE_CXX_FLAGS_RELEASE} -O3")
endif()

target_link_libraries(huffman -lpthread)