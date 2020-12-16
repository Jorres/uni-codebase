#ifndef THREADPOOL_H
#define THREADPOOL_H

#include "request.h"

#include <atomic>
#include <condition_variable>
#include <iostream>
#include <mutex>
#include <queue>
#include <thread>
#include <vector>

#include <QObject>
#include <QString>

class TThreadpool : public QObject {
private:
    Q_OBJECT
public:
    TThreadpool();
    ~TThreadpool();

    void start_new_search_session(const QString& substr, const QString& start_path);
    void cancel_search_session();

    void try_queue_callback();
    Q_INVOKABLE void callback();

    std::vector<location> get_result();

signals:
    void events_added();

private:
    mutable std::mutex queue_mutex;

    size_t threads_amount;

    std::atomic<int32_t> times_canceled;
    std::atomic<bool> global_end;
    std::atomic<bool> remaining;

    std::condition_variable has_work_cv;

    std::vector<std::thread> pool;
    std::vector<location> found_samples;
    std::queue<request> events;

    bool callback_queued;
};

#endif // THREADPOOL_H
