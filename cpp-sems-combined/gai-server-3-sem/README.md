# gai_server

## Description

This is a multithreaded server that accepts text data with urls via socket connections, applies standard C `getaddrinfo` function to the input and returns the result. The most interesting part of the work lies in low-level multithreading mechanics and using `epoll` for socket connection management.

## Usage

You need to clone the repo, run cmake and make, and then run the resulting executable with no addional arguments.

By default 3 servers are launched, on ports 3238, 3239, 3240. Amount of servers and listening ports are configurable.
