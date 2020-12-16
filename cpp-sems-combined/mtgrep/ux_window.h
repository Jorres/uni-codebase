#ifndef UX_WINDOW_H
#define UX_WINDOW_H

#include <QMainWindow>

#include "boss_thread.h"
#include "threadpool.h"

namespace Ui {
class ux_window;
}

class ux_window : public QMainWindow
{
    Q_OBJECT

public:
    explicit ux_window(QWidget *parent = 0);
    ~ux_window();

private:
    Ui::ux_window *ui;
    TThreadpool threadpool;
    // TPrintThread print_thread;
};

#endif // UX_WINDOW_H
