#ifndef PRINT_THREAD_H
#define PRINT_THREAD_H

#include "threadpool.h"

#include <atomic>
#include <condition_variable>
#include <mutex>
#include <thread>

#include <QObject>
#include <QTextEdit>

class TPrintThread : public QObject {
private:
    Q_OBJECT
public:
    TPrintThread();
    ~TPrintThread();

    void relaunch_print();
    void set_data(QTextEdit* textEdit, TThreadpool* threadpool);

private:
    QTextEdit* textEdit;
    std::atomic<bool> global_end;
    std::atomic<bool> printing_mode;
    std::atomic<int> times_canceled;
    TThreadpool* threadpool;
    mutable std::mutex print_mutex;
    std::condition_variable has_work_cv;

    std::thread t;
};


#endif // PRINT_THREAD_H
