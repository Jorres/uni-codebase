#include <iostream>
#include <vector>
#include <set>
#include <fstream>

#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <fcntl.h>
#include <poll.h>
#include <unistd.h>
#include <signal.h>
#include <sys/epoll.h>

using namespace std;

const int BUF_SIZE = 1024;
const int IGNORED_SIZE_FOR_EPOLL_CREATE = 1;

struct epoll_handler {
public:
    epoll_handler() 
        : max_events(100)
    {
        events = new epoll_event[max_events];
        int IGNORED_SIZE = 100;
        fd = epoll_create(IGNORED_SIZE);
    }

    int exec() {
        epoll_event ev;
        ev.events = EPOLLIN | EPOLLET;

        while (true) {
            int ready = epoll_wait(fd, events, max_events, -1);
            if (ready == -1) {
                cout << "epoll_wait failed" << endl;
                return -1;
            }

            cout << ready << " file descriptors ready" << endl;

            for (int i = 0; i < ready; i++) {
                if (connection_listeners.count(events[i].data.fd) > 0) { // NEW_CONNECTION_OCCURED
                    int listener = events[i].data.fd;
                    socklen_t socklen = sizeof(sockaddr_in);
                    sockaddr_in their_addr;
                    int client = accept(listener, (sockaddr *)&their_addr, &socklen);
                    cout << "New client accepted " << client << endl;
                    ev.data.fd = client;
                    int st = epoll_ctl(fd, EPOLL_CTL_ADD, client, &ev);
                    cout << st << endl;
                    clients.insert(client);
                } else {
                    int client = events[i].data.fd;
                    char buf[BUF_SIZE];
                    int r = BUF_SIZE;
                    r = read(client, buf, r);
                    cout << "useful data read" << endl;
    
                    if (r == 0) {
                        epoll_ctl(fd, EPOLL_CTL_DEL, client, &ev); // ev unnecessary, but to avoid kernel bug
                        clients.erase(clients.find(client));
                        close(client);
                    } else {
                        write(client, buf, r);
                    }
                }
            }
        }
    }

    ~epoll_handler() {
        close(fd);
        delete[] events;
    }

    int fd;
    int max_events;

    set<int> connection_listeners;
    set<int> clients;

    epoll_event* events;
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
        cout << "Main listener created " << listener << endl;
    
        bind(listener, (sockaddr const*)&server_addr, sizeof(server_addr));
        cout << "bound" << endl;
    
        listen(listener, SOMAXCONN);
        cout << "Listening started" << endl;
    }

    void bind_to_epoll() {
        epoll_event ev;
        ev.events = EPOLLIN | EPOLLET;

        ev.data.fd = listener;
        epoll_ctl(e_h.fd, EPOLL_CTL_ADD, listener, &ev);
        cout << " binding listener " << listener << endl;
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
    cout << "Epoll fd is " << e.fd << endl;

    server s1(3238, e);
    server s2(3239, e);

    s1.bind_to_epoll();
    s2.bind_to_epoll();

    if (e.exec() == -1) {
        cout << strerror(errno) << endl;
    }

    return 0;
}
