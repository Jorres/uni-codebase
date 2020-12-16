#ifndef SERVER_H
#define SERVER_H

#include "stdlib_includes.h"
#include "config.h"

class epoll_handler;
class client_t;

struct task {
    int client;
    std::string data;
};

class server {
    friend class client_t; // so it can call callbacks
public:
    server() = delete;
    server(int port, epoll_handler& e_h_);
    ~server();

    server(const server&) = delete;
    server(server&&) = delete;

    server operator=(const server&) = delete;
    server operator=(server&&) = delete;

private:
    void init_new_client(int client);
    void process_read_request(client_t& client);
    void process_write_request(client_t& client);

    void refresh_timer(client_t& client);
    void send_reply(client_t& client);
    void append_to_client_request(client_t& client, const char* buf, int bufsize);
    
    void make_getaddrinfo_request(const task&);
    bool transmission_complete(client_t& client);
    bool is_timer_expired(client_t& client);
    bool close_bg_thread();

    epoll_event make_epoll_read_event(client_t& client);
    epoll_event make_epoll_write_event(client_t& client);

private:
    int listener;
    std::map<int, std::function<void(epoll_event_type)>> callbacks;

    epoll_handler& e_h;
    int e_h_fd;

    std::mutex queue_mutex;
      std::map<int, client_t> clients;
      std::queue<task> tasks;

    std::condition_variable cv;
    std::atomic<bool> canceled;

    std::thread bg_thread;
};
#endif // SERVER_H
