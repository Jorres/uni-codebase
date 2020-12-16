#include <iostream>
#include <vector>
#include <algorithm>
#include <set>
#include <fstream>
#include <thread>
#include <mutex>
#include <map>
#include <atomic>
#include <condition_variable>
#include <queue>
#include <cassert>

#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <fcntl.h>
#include <poll.h>
#include <unistd.h>
#include <signal.h>
#include <sys/epoll.h>
#include <stdlib.h>
#include <netdb.h>

using namespace std;

const int BUF_SIZE = 1024;
const int IGNORED_SIZE_FOR_EPOLL_CREATE = 1;
const int EPOLL_WAIT_TIMEOUT = 2000;


namespace {
    template<typename T>
    void log(T elem) {
        cout << elem << endl;
    }
    
    template<typename T, typename... Args>
    void log(T elem, Args... args) {
        cout << elem << " ";
        log(args...);
    }

    std::string ltrim(std::string&& str, const std::string& chars = "\t\n\v\f\r ") {
        return str.erase(0, str.find_first_not_of(chars));
    }
    
    std::string rtrim(std::string& str, const std::string& chars = "\t\n\v\f\r ") {
        return str.erase(str.find_last_not_of(chars) + 1);
    }
    
    std::string trim(std::string str, const std::string& chars = "\t\n\v\f\r ") {
        return ltrim(rtrim(str, chars), chars);
    }
}

struct epoll_handler {
public:
    struct reply {
        bool ready;
        string data;
    };

    struct task {
        int client;
        string data;
    };

    void make_getaddrinfo_request(const task& cur_task, reply& cur_reply) {
        struct addrinfo hints, *res = nullptr;
        int errcode;
      
        memset (&hints, 0, sizeof (hints));
        hints.ai_family = PF_UNSPEC;
        hints.ai_socktype = SOCK_STREAM;
        hints.ai_flags |= AI_CANONNAME;
        errcode = getaddrinfo (cur_task.data.c_str(), NULL, &hints, &res);
      
        if (errcode != 0) {
            cur_reply.data = gai_strerror(errno);
        } else {
            int ip_start = 2;
            int ip_end = 6;
            for (auto p = res; p != NULL; p = p->ai_next) {
                for (int i = ip_start; i < ip_end; i++) {
                    cur_reply.data.append(to_string(static_cast<unsigned char>(p->ai_addr->sa_data[i])));
                    cur_reply.data.push_back('.');
                }
                cur_reply.data.back() = '\n';
            }
        }
        cur_reply.ready = true;
        freeaddrinfo(res);
    }

    epoll_handler() 
        : max_events(100)
        , canceled(false)
        , bg_thread([this] ()
        {
            unique_lock<mutex> lg(queue_mutex);
            
            while (true) {
                cv.wait(lg, [this] () {
                    log("awakening");
                    return !tasks.empty() || canceled;
                });

                if (canceled) {
                    break;
                }

                while (!tasks.empty()) {
                    task cur_task = tasks.front();
                    tasks.pop();
                    lg.unlock();

                    reply cur_reply;
                    make_getaddrinfo_request(cur_task, cur_reply);
                    lg.lock();
                    replies[cur_task.client] = cur_reply;
                }
            }
        })
    {
        events = new epoll_event[max_events];
        int IGNORED_SIZE = 100;
        fd = epoll_create(IGNORED_SIZE);
    }

    int exec() {
        epoll_event ev_write_mask;
        ev_write_mask.events = EPOLLOUT | EPOLLET;
        epoll_event ev_read_mask;
        ev_read_mask.events = EPOLLIN | EPOLLET;

        while (true) {
            int ready = epoll_wait(fd, events, max_events, EPOLL_WAIT_TIMEOUT);
            if (ready == -1) {
                log("epoll_wait failed");
                return -1;
            }

            log(ready, "file descriptors ready");
            for (int i = 0; i < ready; i++) {
                if (connection_listeners.count(events[i].data.fd) > 0) { // NEW_CONNECTION_OCCURED
                    int listener = events[i].data.fd;
                    socklen_t socklen = sizeof(sockaddr_in);
                    sockaddr_in their_addr;
                    int client = accept(listener, (sockaddr *)&their_addr, &socklen);
                    log("new client accepted", client);
                    ev_read_mask.data.fd = client;
                    int st = epoll_ctl(fd, EPOLL_CTL_ADD, client, &ev_read_mask);
                    clients.insert(client);
                    replies[client] = {
                        false, ""
                    };
                } else if ((events[i].events & EPOLLIN) != 0) {
                    int client = events[i].data.fd;
                    char buf[BUF_SIZE];
                    int r = BUF_SIZE;
                    r = read(client, buf, r);
                    for (int j = 0; j < r; j++) {
                        cout << buf[j];
                    }
                    cout << endl;
                    ev_write_mask.data.fd = client;

                    string task_data(buf, r);
                    task_data = trim(task_data);
                    {
                        lock_guard<mutex> lg(queue_mutex);
                        tasks.push(task{
                            client, task_data
                        });
                        cv.notify_one();
                    }

                   
                    if (r > 0) {
                        epoll_ctl(fd, EPOLL_CTL_MOD, client, &ev_write_mask);
                    } else if (r == 0) {
                        epoll_ctl(fd, EPOLL_CTL_DEL, client, &ev_write_mask); // ev unnecessary, but to avoid kernel bug
                        clients.erase(clients.find(client));
                        close(client);
                    } 
                } else if ((events[i].events & EPOLLOUT) != 0) {
                    int client = events[i].data.fd;
                    awaiting_reply.insert(client);
                }
            }

            vector<int> deleted;
            {
                lock_guard<mutex> lg(queue_mutex);
                for (auto client : awaiting_reply) {
                    if (replies[client].ready) {
                        log("inside replying");
                        write(client, replies[client].data.c_str(), replies[client].data.size());
                        deleted.push_back(client);
    
                        ev_read_mask.data.fd = client;
                        epoll_ctl(fd, EPOLL_CTL_MOD, client, &ev_read_mask);
                    }
                }
            }

            for (auto client : deleted) {
                awaiting_reply.erase(client);
            }
        }
    }

    ~epoll_handler() {
        close(fd);
        delete[] events;
        canceled.store(true);
        assert(bg_thread.joinable());
        bg_thread.join();
    }

    int fd;
    int max_events;


    set<int> connection_listeners;
    set<int> clients;
    set<int> awaiting_reply;

    epoll_event* events;
    
    map<int, reply> replies;

    condition_variable cv;
    mutex queue_mutex;
    atomic<bool> canceled;
    queue<task> tasks;

    thread bg_thread;
};

struct server {
public:
    server(int port, epoll_handler& e_h_)
        : e_h(e_h_)
    {
        sockaddr_in server_addr;
        server_addr.sin_family = PF_INET;
        server_addr.sin_port = htons(port); // server port carry to arguments
        server_addr.sin_addr.s_addr = 0; // seems to be inet connected

        listener = socket(PF_INET, SOCK_STREAM, 0);
        log("main listener created", listener);
    
        bind(listener, (sockaddr const*)&server_addr, sizeof(server_addr));
        log("bound");
    
        listen(listener, SOMAXCONN);
        log("listening started");
    }

    void bind_to_epoll() {
        epoll_event ev;
        ev.events = EPOLLIN | EPOLLET;

        ev.data.fd = listener;
        epoll_ctl(e_h.fd, EPOLL_CTL_ADD, listener, &ev);
        log("binding listener", listener);
        e_h.connection_listeners.insert(listener);
    }

    ~server() {
        close(listener);
    }

    int listener;
    epoll_handler& e_h;
};

int main() {
    epoll_handler e;

    if (e.fd == -1) {
        cout << strerror(errno) << endl;
        return 0;
    }
    log("epoll fd is", e.fd);

    server s1(3238, e);
    server s2(3239, e);

    s1.bind_to_epoll();
    s2.bind_to_epoll();

    if (e.exec() == -1) {
        cout << strerror(errno) << endl;
    }

    return 0;
}
