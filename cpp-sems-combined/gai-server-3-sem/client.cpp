#include "stdlib_includes.h"
#include "epoll_and_sockets.h"

#include "client.h"
#include "helpers.h"

client_t::client_t() 
    : fd(-1)
    , reply(reply_t{false, ""})
    , callback_ptr(nullptr)
    , timer_callback_ptr(nullptr)
    , prefix("")
    , awaiting_reply(false)
    , timer(-1) { 
    }

client_t::client_t(int client, server* serv_) : client_t() {
    serv = serv_;
    fd = client;

    serv->callbacks.emplace(std::make_pair(client, [client, serv_] (epoll_event_type mode) {
        if (mode == epoll_event_type::READ) {
            serv_->process_read_request(serv_->clients[client]);
        } else if (mode == epoll_event_type::WRITE) {
            serv_->process_write_request(serv_->clients[client]);
        } else if (mode == epoll_event_type::DELETE) {
            serv_->clients.erase(client);
        }
    }));

    callback_ptr = reinterpret_cast<void*>(&(serv->callbacks[client]));
    epoll_event event;
    event.events = EPOLLIN | EPOLLET;
    event.data.ptr = callback_ptr;
    exception_wrap(epoll_ctl(serv->e_h_fd, EPOLL_CTL_ADD, client, &event));

    log("creating new timer for", client);
    exception_wrap(timer = timerfd_create(CLOCK_MONOTONIC, 0));
    serv->refresh_timer(*this);

    serv->callbacks.emplace(std::make_pair(timer, [serv_, client] (epoll_event_type ignored) {
        serv_->process_read_request(serv_->clients[client]);
    }));

    timer_callback_ptr = reinterpret_cast<void*>(&(serv->callbacks[timer]));
    epoll_event timer_event;
    timer_event.data.ptr = timer_callback_ptr;
    timer_event.events = EPOLLIN | EPOLLET;
    exception_wrap(epoll_ctl(serv->e_h_fd, EPOLL_CTL_ADD, timer, &timer_event));

    log("timer for client", client, "created and equals", timer);
}

client_t::~client_t() {
    log("deleting client", fd);

    serv->callbacks.erase(timer);
    exception_wrap(epoll_ctl(serv->e_h_fd, EPOLL_CTL_DEL, timer, NULL));
    close(timer);

    serv->callbacks.erase(fd);
    exception_wrap(epoll_ctl(serv->e_h_fd, EPOLL_CTL_DEL, fd, NULL));
    close(fd);

    log("safely deleted");
}
