cmake_minimum_required(VERSION 3.10)
project(server)

set(CMAKE_CXX_STANDARD 17)
set(CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -fsanitize=address,undefined -Wall -fmax-errors=10 -pthread")

set(REQ_FILES
        epoll_and_sockets.h
        stdlib_includes.h
        config.h
        helpers.cpp
        helpers.h
        server.cpp
        server.h
        client.cpp
        client.h
        epoll_handler.cpp
        epoll_handler.h
        )

add_executable(server main.cpp ${REQ_FILES})
