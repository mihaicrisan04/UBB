#include "window.hpp"

#include <iostream>
#include <fstream>

void Window::initGUI() {
    setWindowTitle(QString::fromStdString(researcher.name));
    mainLayout = new QVBoxLayout(this);

    positionLabel = new QLabel(QString::fromStdString(researcher.position));

    std::vector<Idea> ideas = session.getIdeas();
    ideasTable = new QTableView();
    model = new IdeaModel(session);
    ideasTable->setModel(model);
    ideasTable->resizeColumnsToContents();

    // Add idea widget
    addIdeaWidget = new QWidget();
    addIdeaLayout = new QHBoxLayout(addIdeaWidget);
    titleEdit = new QLineEdit();
    titleEdit->setPlaceholderText("Title");
    descriptionEdit = new QLineEdit();
    descriptionEdit->setPlaceholderText("Description");
    durationEdit = new QLineEdit();
    durationEdit->setPlaceholderText("Duration");
    addIdeaButton = new QPushButton("Add idea");

    addIdeaLayout->addWidget(titleEdit);
    addIdeaLayout->addWidget(descriptionEdit);
    addIdeaLayout->addWidget(durationEdit);
    addIdeaLayout->addWidget(addIdeaButton);


    mainLayout->addWidget(positionLabel);
    mainLayout->addWidget(ideasTable);
    mainLayout->addWidget(addIdeaWidget);

    // senior researcher can accept ideas
    if (researcher.position == "senior") {
        acceptButton = new QPushButton("Accept idea");
        saveAllButton = new QPushButton("Save all");

        mainLayout->addWidget(acceptButton);
        mainLayout->addWidget(saveAllButton);
    }

    for (auto &idea : ideas) {
        if (idea.status == "accepted" && idea.creator == researcher.name) {
            hasAcceptedIdeas = true;
            break;
        }
    }
    if (hasAcceptedIdeas) {
        developButton = new QPushButton("Develop");
        mainLayout->addWidget(developButton);
    }

    setLayout(mainLayout);
}

void Window::connectSignals() {
    connect(addIdeaButton, &QPushButton::clicked, this, &Window::addIdea);
    if (researcher.position == "senior") { 
        connect(acceptButton, &QPushButton::clicked, this, &Window::acceptIdea);
        connect(saveAllButton, &QPushButton::clicked, this, &Window::saveAll);
    }
    if (hasAcceptedIdeas) {
        connect(developButton, &QPushButton::clicked, this, &Window::developIdea);
    }
}

void Window::update() {
    model->update();
}

void Window::addIdea() {
    std::string title = titleEdit->text().toStdString();
    std::string description = descriptionEdit->text().toStdString();
    int duration = durationEdit->text().toInt();

    session.addIdea(Idea(title, description, "proposed", researcher.name, duration));

    titleEdit->clear();
    descriptionEdit->clear();
    durationEdit->clear();
}

void Window::acceptIdea() {
    QModelIndex index = ideasTable->currentIndex();
    if (index.isValid()) {
        Idea idea = session.getIdeas()[index.row()];
        if (idea.status == "proposed") {
            session.updateIdeaStatus(idea, "accepted");
        }
    }
}

void Window::saveAll() {
    std::vector<Idea> ideas = session.getIdeas();
    std::ofstream file ("../acceptedIdeas.txt");
    for (auto &idea : ideas) {
        if (idea.status == "accepted") {
            file << idea.toString() << std::endl;
        }
    } 
    file.close();
}

#include <QDialog>

void Window::developIdea() {
    std::vector<Idea> ideas = session.getIdeas();
    for (auto &idea : ideas) {
        if (idea.status == "accepted" && idea.creator == researcher.name) {
            QDialog *dialog = new QDialog();
            QVBoxLayout *layout = new QVBoxLayout(dialog);
            QLabel *label = new QLabel(QString::fromStdString(idea.description));
            QPushButton *saveButton = new QPushButton("Save");
            layout->addWidget(label);
            layout->addWidget(saveButton);
            dialog->setLayout(layout);
            dialog->exec();
        }
    }
}