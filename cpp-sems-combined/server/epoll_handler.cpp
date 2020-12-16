#include "stdlib_includes.h"
#include "epoll_and_sockets.h"

#include "config.h"
#include "epoll_handler.h"
#include "helpers.h"
#include "server.h"

volatile bool* quit = nullptr;

void signal_handler(int ignored) {
    if (!quit) {
        return;
    }

    *quit = true;
}

epoll_handler::epoll_handler()
    : max_events(MAX_EPOLL_EVENTS)
{
    events = new epoll_event[max_events];
    int IGNORED_SIZE = 100;
    exception_wrap(fd = epoll_create(IGNORED_SIZE));
    log("epoll fd is", fd);
}

using callback_type = std::function<void(epoll_event_type)>;

int epoll_handler::exec() {
    std::signal(SIGINT, signal_handler);
    std::signal(SIGTERM, signal_handler);
    bool interrupted = false;
    quit = &interrupted;

    while (true) {
        int ready;
        exception_wrap(ready = epoll_wait(fd, events, max_events, EPOLL_WAIT_TIMEOUT_MILLISECONDS));

        if (interrupted) {
            break;
        }

        for (int i = 0; i < ready; i++) {
            void*    cur_ptr    = events[i].data.ptr;
            uint32_t cur_events = events[i].events;

            if ((cur_events & EPOLLIN) != 0) {
                (*reinterpret_cast<callback_type*>(cur_ptr))(epoll_event_type::READ);
            } else if ((cur_events & EPOLLOUT) != 0) {
                (*reinterpret_cast<callback_type*>(cur_ptr))(epoll_event_type::WRITE);
            } else if ((cur_events & EPOLL_CLOSE_EVENTS) != 0) {
                (*reinterpret_cast<callback_type*>(cur_ptr))(epoll_event_type::DELETE);
            }
        }
    }

    return 0;
}

epoll_handler::~epoll_handler() {
    close(fd);
    delete[] events;
}
