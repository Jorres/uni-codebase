#ifndef CONFIG_H
#define CONFIG_H

#define LOG_ENABLED

const int STARTING_PORT = 3238;
const int BUF_SIZE = 1024;
const int EPOLL_WAIT_TIMEOUT_MILLISECONDS = 1 * 1000;
const int EPOLL_BLOCK_INDEFINITELY = -1;
const int MAX_CLIENT_INACTIVITY_DURATION_SECONDS = 30;
const int MAX_REQUEST_LENGTH = 1024;
const int MAX_EPOLL_EVENTS = 100;

enum class epoll_event_type {
    READ, WRITE, DELETE
};

#endif
