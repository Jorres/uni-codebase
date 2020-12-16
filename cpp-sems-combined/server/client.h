#ifndef CLIENT_H
#define CLIENT_H

#include "stdlib_includes.h"
#include "server.h"

struct reply_t {
    bool ready;
    std::string data;
};

struct client_t {
    client_t();
    client_t(int client, server* serv);
    ~client_t();

    int fd;
    reply_t reply;
    void* callback_ptr;
    void* timer_callback_ptr;
    std::string prefix;
    bool awaiting_reply;
    int timer;
    server* serv;
};

#endif // CLIENT_H
