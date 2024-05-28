#include "gui.hpp"


GUI::GUI(std::string filename) : serv(filename) {
    initGUI();
}

GUI::~GUI() {}

void GUI::initGUI() {
    centralWidget = new QWidget();
    mainLayout = new QHBoxLayout(centralWidget);

    tasksList = new QListWidget;


    durationWidget = new QWidget();
    durationLayout = new QVBoxLayout(durationWidget);

    inputDurationLineEdit = new QLineEdit();
    computeDurationButton = new QPushButton("Hatz");
    responseLabel = new QLabel();

    durationLayout->addWidget(inputDurationLineEdit);
    durationLayout->addWidget(computeDurationButton);
    durationLayout->addWidget(responseLabel);


    mainLayout->addWidget(tasksList);
    mainLayout->addWidget(durationWidget);

    setCentralWidget(centralWidget);
    connectSingalsAndSlots();
    populateList();
}

void GUI::connectSingalsAndSlots() {
    connect(computeDurationButton, &QPushButton::clicked, this, [this]() {
        std::vector<Task> tasks = serv.getTasks();
        std::string priorityStr = inputDurationLineEdit->text().toStdString();
        int priority = stoi(priorityStr);

        int totalTime = 0;
        for (auto t: tasks) {
            if (t.priority == priority) {
                totalTime += t.duration;
            }
        }

        responseLabel->setText(QString::fromStdString(std::to_string(totalTime)));
    });
}

void GUI::populateList() {
    std::vector<Task> tasks = serv.getTasks();
    sort(tasks.begin(), tasks.end(), [](const Task& a, const Task& b) {
        return a.priority < b.priority;
    });

    for (auto t: tasks) {
        std::string descriptionAndPriority = t.description + " - " + std::to_string(t.priority);
        QListWidgetItem *item = new QListWidgetItem(QString::fromStdString(descriptionAndPriority));
        if (t.priority == 3) {
            QFont font;
            font.setBold(true);
            item->setFont(font);
        }

        tasksList->addItem(item);
    }
}
