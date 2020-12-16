#include "stdlib_includes.h"
#include "epoll_and_sockets.h"

#include "network_exception.h"
#include "epoll_handler.h"
#include "helpers.h"
#include "server.h"

int main() {
    epoll_handler e;

    int servers_amount = 3;
    std::list<server> servers;
    for (int i = 0; i < servers_amount; i++) {
        servers.emplace_back(STARTING_PORT + i, e);
    }

    try {
        e.exec();
    } catch (network::exception const& ex) {
        log(ex.what());
    }

    return 0;
}
