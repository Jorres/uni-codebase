#include "threadpool.h"

#include <algorithm>
#include <cassert>
#include <cstring>
#include <fstream>

#include <QDir>

namespace {
    std::string ltrim(std::string&& str, const std::string& chars = "\t\n\v\f\r ") {
        return str.erase(0, str.find_first_not_of(chars));
    }

    std::string rtrim(std::string& str, const std::string& chars = "\t\n\v\f\r ") {
        return str.erase(str.find_last_not_of(chars) + 1);
    }

    std::string trim(std::string str, const std::string& chars = "\t\n\v\f\r ") {
        return ltrim(rtrim(str, chars), chars);
    }

    bool result_relevant(int32_t last, int32_t current) {
        return last == current;
    }

    bool exists_and_readable(const std::string& path) {
        if (FILE* f = fopen(path.c_str(), "r")) {
            fclose(f);
            return true;
        }
        return false;
    }
    
    void cout_tracing(const std::string& s) {
        //std::cout << s << std::endl;
    }
}

TThreadpool::TThreadpool()
    : threads_amount(2)
    , times_canceled(false)
    , global_end(false)
    , remaining(false)
    , callback_queued(false)
    {
    for (size_t i = 0; i < threads_amount; i++) {
        int32_t thread_num = i;
        pool.emplace_back([this, thread_num] {
            std::unique_lock<std::mutex> lg(queue_mutex);
            int32_t last_canceled = 1;
            bool take_next = false;

            while (true) {
                if (global_end) {
                    break; // for thos stopped in the middle of search
                }

                if (!take_next) {
                    has_work_cv.wait(lg, [this] {
                        return (events.size() > 0 || global_end || remaining);
                    });
                }

                if (global_end) {
                    break; // for those stopped while waiting
                }

                if (remaining) {
                    try_queue_callback();
                    remaining = false;
                    continue;
                }

                last_canceled = times_canceled;

                request data = events.front();
                events.pop();

                cout_tracing("thread num " + std::to_string(thread_num) + " launched " + data.start_path);

                lg.unlock(); // there goes heavy work

                std::string path = data.start_path;
                if (path.back() == '/') {
                    path.pop_back();
                }

                QDir dir(QString::fromStdString(path));

                std::string line;
                std::vector<std::string> paths;
                std::vector<location> locations;

                if (dir.exists()) {
                    dir.setFilter(QDir::Files | QDir::Dirs | QDir::NoDotAndDotDot);
                    QFileInfoList list = dir.entryInfoList();
                    for (auto& entry : list) {
                        std::string name = entry.fileName().toStdString();
                        paths.push_back(name);
                    }
                    has_work_cv.notify_all();
                } else {
                    if (exists_and_readable(path)) {
                        std::ifstream file;
                        file.open(path);
                        if (!file.good()) {
                            cout_tracing("file " + path + " failed to open");
                            lg.lock();
                            continue;
                        }

                        size_t line_num = 0;
                        while (std::getline(file, line)) {
                            line_num++;
                            cout_tracing("attempting to search for *" + data.substr + "* in *" + line + "*");

                            if (!result_relevant(last_canceled, times_canceled)) {
                                break;
                            }

                            for (int32_t i = 0; i <= static_cast<int32_t>(line.size()) - static_cast<int32_t>(data.substr.size()); i++) {
                                bool found = true;
                                if (!result_relevant(last_canceled, times_canceled)) {
                                    break;
                                }
                                for (size_t j = i; j < i + data.substr.size() && found; j++) {
                                    if (line[j] != data.substr[j - i]) {
                                        found = false;
                                    }
                                }
                                if (found) {
                                    locations.emplace_back(location(path, line_num, i));
                                }
                            }
                        }
                        file.close();
                        cout_tracing("string search complete");
                    } else {
                        locations.emplace_back(location(path, /* success := */ false));
                    }
                }

                lg.lock(); // ensure mutex isn't being held for long

                for (const auto& next_level_path : paths) {
                    if (result_relevant(last_canceled, times_canceled)) {
                        events.push(request(data.substr, path + "/" + next_level_path));
                    }
                }
                paths.clear();

                for (const auto& result : locations) {
                    if (result_relevant(last_canceled, times_canceled)) {
                        found_samples.push_back(result);
                    }
                }
                locations.clear();

                try_queue_callback();
                take_next = events.size() > 0;
                cout_tracing("thread num " + std::to_string(thread_num) + " exited");
            }
        });
    }
}

void TThreadpool::start_new_search_session(const QString& substr, const QString& start_path) {
    cancel_search_session();

    std::lock_guard<std::mutex> lg(queue_mutex);

    std::string std_substr = substr.toStdString();

    if (std_substr.empty()) {
        return;
    }

    std::string std_path = start_path.toStdString();

    events.push(request(trim(std_substr), trim(std_path)));
    assert(events.size() == 1);

    has_work_cv.notify_all();
}

void TThreadpool::cancel_search_session() {
    std::lock_guard<std::mutex> lg(queue_mutex);
    times_canceled++;
    remaining = false;
    found_samples.clear();
    std::queue<request>().swap(events);
}

TThreadpool::~TThreadpool() {
    global_end.store(true);
    times_canceled++;
    has_work_cv.notify_all();
    for (size_t i = 0; i < threads_amount; i++) {
        if (pool[i].joinable()) {
            pool[i].join();
        }
    }
}

std::vector<location> TThreadpool::get_result() {
    std::lock_guard<std::mutex> lg(queue_mutex);
    std::vector<location> result;
    size_t batch_size = 50;
    size_t to = std::min(found_samples.size(), batch_size);
    for (size_t i = 0; i < to; ++i) {
        result.push_back(found_samples.back());
        found_samples.pop_back();
    }
    remaining = found_samples.size() > 0;
    if (remaining) {
        has_work_cv.notify_one();
    }
    return result;
}

void TThreadpool::try_queue_callback() {
    if (callback_queued) {
        return;
    }

    callback_queued = true; // at every time moment at most 1 callback is queued

    QMetaObject::invokeMethod(this, "callback", Qt::QueuedConnection);
}

void TThreadpool::callback() {
    {
        std::unique_lock<std::mutex> lg(queue_mutex);
        callback_queued = false;
    } // why cannot emit under lock guard? necessary or not? upd not

    emit events_added();
}
