#ifndef EPOLL_HANDLER_H
#define EPOLL_HANDLER_H

#include "stdlib_includes.h"
#include "epoll_and_sockets.h"

#include "server.h"
#include "config.h"

class epoll_handler {
public:
    epoll_handler();
    ~epoll_handler();

    int exec();

public:
    int fd;

    int max_events;
    epoll_event* events;

private:
    uint32_t EPOLL_CLOSE_EVENTS = (EPOLLERR | EPOLLRDHUP | EPOLLHUP);
};

#endif // EPOLL_HANDLER_H
