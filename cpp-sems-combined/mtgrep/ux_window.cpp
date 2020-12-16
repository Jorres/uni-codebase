#include "ux_window.h"
#include "ui_ux_window.h"

ux_window::ux_window(QWidget *parent) :
    QMainWindow(parent),
    ui(new Ui::ux_window)
{
    ui->setupUi(this);

    ui->lineEdit_2->insert("/home/jorres/work/prg/cpp/qt/multithread_grep/test_dir/");
    ui->lineEdit->insert("abc");
    connect(ui->lineEdit_2, &QLineEdit::textChanged, this, [this] () {
        ui->textEdit->setPlainText("");
        threadpool.start_new_search_session(ui->lineEdit->text(), ui->lineEdit_2->text());
    });

    connect(ui->lineEdit, &QLineEdit::textChanged, this, [this] () {
        ui->textEdit->setPlainText("");
        if (ui->lineEdit->text() == "") {
            ui->textEdit->setPlainText("Empty substring, search aborted");
        }
        threadpool.start_new_search_session(ui->lineEdit->text(), ui->lineEdit_2->text());
    });

    connect(&threadpool, &TThreadpool::events_added, this, [this] { 
        std::vector<location> locations = threadpool.get_result();
        for (auto& location : locations) {
            if (location.success) {
                ui->textEdit->appendPlainText(QString::fromStdString(location.path + ":" +
                                                        std::to_string(location.line_num) + ":" +
                                                        std::to_string(location.line_pos)));
            } else {
                ui->textEdit->appendPlainText(QString::fromStdString("Target location " + location.path +
                                                        " does not exist or is not accessible"));
            }
        }
    });
}

ux_window::~ux_window()
{
    delete ui;
}
