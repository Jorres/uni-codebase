#include "stdlib_includes.h"
#include "epoll_and_sockets.h"

#include "config.h"
#include "client.h"
#include "helpers.h"
#include "epoll_handler.h"
#include "server.h"

epoll_event server::make_epoll_read_event(client_t& client) {
    epoll_event event;
    event.data.ptr = client.callback_ptr;
    event.events = EPOLLIN | EPOLLET;
    return event;
}

epoll_event server::make_epoll_write_event(client_t& client) {
    epoll_event event;
    event.data.ptr = client.callback_ptr;
    event.events = EPOLLOUT | EPOLLET;
    return event;
}

void server::send_reply(client_t& client) {
    // under response_mutex everywhere
    if (clients.count(client.fd)) {
        exception_wrap(write(client.fd, client.reply.data.c_str(), client.reply.data.size()));
        epoll_event ev_read_event = make_epoll_read_event(client);
        log(client.fd, client.callback_ptr, "data");
        exception_wrap(epoll_ctl(e_h.fd, EPOLL_CTL_MOD, client.fd, &ev_read_event));

        client.awaiting_reply = false;
        client.reply = reply_t{ false, "" };
    }
}

void server::make_getaddrinfo_request(const task& cur_task) {
    struct addrinfo hints, *res = nullptr;
    memset (&hints, 0, sizeof(hints));
    hints.ai_family = PF_UNSPEC;
    hints.ai_socktype = SOCK_STREAM;
    hints.ai_flags |= AI_CANONNAME;
    int errcode = getaddrinfo(cur_task.data.c_str(), NULL, &hints, &res);

    reply_t cur_reply;

    if (errcode != 0) {
        cur_reply.data = gai_strerror(errno);
        cur_reply.data.push_back('\n');
    } else {
        const int ip_start = 2; // experimentally set constants
        const int ip_end = 6;
        for (auto p = res; p != NULL; p = p->ai_next) {
            for (int i = ip_start; i < ip_end; i++) {
                cur_reply.data.append(std::to_string(static_cast<unsigned char>(p->ai_addr->sa_data[i])));
                cur_reply.data.push_back('.');
            }
            cur_reply.data.back() = '\n';
        }
    }
    cur_reply.ready = true;
    freeaddrinfo(res);

    {
        std::lock_guard<std::mutex> lg(queue_mutex);
        if (clients.count(cur_task.client)) {
            clients[cur_task.client].reply = cur_reply;
            if (clients[cur_task.client].awaiting_reply == true) {
                send_reply(clients[cur_task.client]);
            }
        }
    }
}

server::server(int port, epoll_handler& e_h_)
    : e_h(e_h_)
    , canceled(false)
    , bg_thread([this] ()
    {
        std::unique_lock<std::mutex> lg(queue_mutex);

        while (true) {
            cv.wait(lg, [this] () {
                return !tasks.empty() || canceled;
            });

            if (canceled) {
                break;
            }

            while (!tasks.empty()) {
                task cur_task = tasks.front();
                tasks.pop();

                if (clients.count(cur_task.client) == 0) {
                    continue;
                }

                lg.unlock();
                make_getaddrinfo_request(cur_task);
                lg.lock();

                log("getaddrinfo result got");
            }
        }
    })
{
    sockaddr_in server_addr;
    server_addr.sin_family = PF_INET;
    server_addr.sin_port = htons(port);
    server_addr.sin_addr.s_addr = 0;

    while (true) {
        if ((listener = socket(PF_INET, SOCK_STREAM, 0)) != -1) {
            break;
        }
        sleep(1);
        log("socket creation failed, restarting...");
    }
    while (true) {
        if (bind(listener, reinterpret_cast<sockaddr const*>(&server_addr), sizeof(server_addr)) != -1) {
            break;
        }
        sleep(1);
        log("socket binding failed, restarting...");
    }
    log("main listener created", listener);

    exception_wrap(listen(listener, SOMAXCONN));
    log("listening started");

    callbacks.emplace(std::make_pair(listener, [this](epoll_event_type ignored) {
        socklen_t socklen = sizeof(sockaddr_in);
        sockaddr_in their_addr;

        int client;
        exception_wrap(client = accept(listener, reinterpret_cast<sockaddr *>(&their_addr), &socklen));

        init_new_client(client);
    }));

    epoll_event event;
    event.data.ptr = reinterpret_cast<void*>(&(callbacks[listener]));
    event.events = EPOLLIN | EPOLLET;

    exception_wrap(epoll_ctl(e_h.fd, EPOLL_CTL_ADD, listener, &event));

    e_h_fd = e_h.fd;
}

void server::refresh_timer(client_t& client) {
    itimerspec tspec;
    tspec.it_value.tv_sec  = MAX_CLIENT_INACTIVITY_DURATION_SECONDS;
    tspec.it_value.tv_nsec = 0;

    tspec.it_interval.tv_sec  = 0;
    tspec.it_interval.tv_nsec = 0;

    timerfd_settime(client.timer, 0, &tspec, NULL);
    // specifics of timerfd - no exception wrap
}

bool server::is_timer_expired(client_t& client) {
    itimerspec tspec;
    std::lock_guard<std::mutex> lg(queue_mutex);
    timerfd_gettime(client.timer, &tspec);
    return tspec.it_value.tv_sec == 0 && tspec.it_value.tv_nsec == 0;
}

void server::init_new_client(int client) {
    std::lock_guard<std::mutex> lg(queue_mutex);
    log("accepting new client", client);
    // Oh god I love this
    clients.emplace(std::piecewise_construct, std::forward_as_tuple(client), std::forward_as_tuple(client, this));
    log("new_client_accepted");
}

bool server::transmission_complete(client_t& client) {
    std::lock_guard<std::mutex> lg(queue_mutex);
    return client.prefix.back() == '\n';
}

void server::append_to_client_request(client_t& client, const char* buf, const int bufsize) {
    int prev_len = client.prefix.size();
    client.prefix.resize(prev_len + bufsize);
    for (int i = 0; i < bufsize; i++) {
        client.prefix[i + prev_len] = buf[i];
    }
}

void server::process_read_request(client_t& client) {
    log("starting request processing");
    if (is_timer_expired(client)) {
        log("timer has been caught for", client.timer);
        std::lock_guard<std::mutex> lg(queue_mutex);
        clients.erase(client.fd);
        log("client deleted");
        return;
    }

    char buf[BUF_SIZE];
    int r = BUF_SIZE;
    exception_wrap(r = read(client.fd, buf, r));
    epoll_event ev_write_event;
    {
        std::lock_guard<std::mutex> lg(queue_mutex);
        ev_write_event = make_epoll_write_event(client);
        refresh_timer(client);

        if (r + client.prefix.size() >= MAX_REQUEST_LENGTH) {
            client.reply.ready = true;
            client.reply.data = "Too long request, keep it simple";
            exception_wrap(epoll_ctl(e_h.fd, EPOLL_CTL_MOD, client.fd, &ev_write_event));
            return;
        }
    }
    log("read complete");

    if (r > 0) {
        append_to_client_request(client, buf, r);
        if (transmission_complete(client)) {
            exception_wrap(epoll_ctl(e_h.fd, EPOLL_CTL_MOD, client.fd, &ev_write_event));
        } else {
            return;
        }
    } else if (r == 0) {
        std::lock_guard<std::mutex> lg(queue_mutex);
        clients.erase(client.fd);
        return;
    }

    std::string task_data = trim(client.prefix);
    {
        std::lock_guard<std::mutex> lg(queue_mutex);
        client.prefix.clear();
        log("task_data", task_data);
        tasks.push(task{ client.fd, std::move(task_data) });
        cv.notify_one();
        log("request processing finished");
    }
}

void server::process_write_request(client_t& client) {
    log("processing write request for", client.fd);
    {
        std::lock_guard<std::mutex> lg(queue_mutex);
        refresh_timer(client);
        log("refreshed timer");
        if (client.reply.ready) {
            log("sending delayed reply...");
            send_reply(client);
        } else {
            client.awaiting_reply = true;
        }
    }
    log("write request processed");
}

bool server::close_bg_thread() {
    canceled.store(true);
    cv.notify_one();
    if (bg_thread.joinable()) {
        bg_thread.join();
        return true;
    }
    return false;
}

server::~server() {
    close(listener);
    bool closed = close_bg_thread();
    assert(closed);
}
