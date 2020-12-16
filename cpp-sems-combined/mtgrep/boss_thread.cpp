#include "boss_thread.h"

bool result_relevant(int last, int current) {
    return last == current;
}

TPrintThread::TPrintThread()
    : global_end(false)
    , printing_mode(false)
    , times_canceled(0)
    , t([this] () {
        std::unique_lock<std::mutex> lg(print_mutex);
        int last_canceled = 1;
        std::cout << "thread launched" << std::endl;
        while (true) {
            if (global_end) {
               break;
            }

            if (!printing_mode) {
                has_work_cv.wait(lg, [this] {
                   return printing_mode || global_end;
                });
            }

            if (global_end) {
               break;
            }

            last_canceled = times_canceled;

            lg.unlock();

            std::cout << "print initiated" << std::endl;
            auto locations = threadpool->get_result();
            for (size_t i = 0; i < locations.size() && result_relevant(last_canceled, times_canceled); i++) {
                auto& location = locations[i];
                if (location.success) {
                    textEdit->append(QString::fromStdString(location.path + ":" +
                                                            std::to_string(location.line_num) + ":" +
                                                            std::to_string(location.line_pos)));
                } else {
                    textEdit->append(QString::fromStdString("Target location " + location.path +
                                                            " does not exist or is not accessible"));
                }

            }
            std::cout << "print end" << std::endl;
            if (result_relevant(last_canceled, times_canceled)) {
                printing_mode.store(false);
            }

            lg.lock();
        }
    })
    {}

void TPrintThread::relaunch_print() {
    printing_mode.store(true);
    times_canceled++;
    has_work_cv.notify_one();
}

void TPrintThread::set_data(QTextEdit* textEdit_, TThreadpool* threadpool_) {
    threadpool = threadpool_;
    textEdit = textEdit_;
}

TPrintThread::~TPrintThread() {
    global_end.store(true);
    if (t.joinable()) {
        t.join();
    }
}
