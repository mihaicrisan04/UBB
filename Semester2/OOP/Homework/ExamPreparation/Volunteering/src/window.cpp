#include "window.hpp"

#include <algorithm>
#include <sstream>

Window::Window(Service &service, Department department, QWidget* parent) : service{service}, department{department}, QWidget{parent} {
    this->initGUI();
    this->connectSignals();
}

void Window::initGUI() {
    service.registerObserver(this);
    this->setWindowTitle(QString::fromStdString(department.name));

    centralWidget = new QWidget;
    layout = new QVBoxLayout{centralWidget};

    description = new QLabel(QString::fromStdString(department.description));
    layout->addWidget(description);

    // Volunteers lists widget
    volunteersWidget = new QWidget;
    volunteersLayout = new QHBoxLayout{volunteersWidget};
    volunteersList = new QListWidget;
    unassignedList = new QListWidget;

    volunteersLayout->addWidget(volunteersList);
    volunteersLayout->addWidget(unassignedList);

    // Add volunteer widget
    addVolunteerWidget = new QWidget;
    addVolunteerLayout = new QHBoxLayout{addVolunteerWidget};
    nameEdit = new QLineEdit;
    emailEdit = new QLineEdit;
    interestsEdit = new QLineEdit;
    addVolunteerButton = new QPushButton{"Add"};

    addVolunteerLayout->addWidget(nameEdit);
    addVolunteerLayout->addWidget(emailEdit);
    addVolunteerLayout->addWidget(interestsEdit);
    addVolunteerLayout->addWidget(addVolunteerButton);

    // Assign volunteer button
    assignVolunteerButton = new QPushButton{"Assign"};

    // Search widget
    searchWidget = new QWidget;
    searchLayout = new QHBoxLayout{searchWidget};
    searchButton = new QPushButton{"Search"};
    response = new QLabel;

    searchLayout->addWidget(searchButton);
    searchLayout->addWidget(response);

    // Add widgets to layout
    layout->addWidget(volunteersWidget);
    layout->addWidget(addVolunteerWidget);
    layout->addWidget(assignVolunteerButton);
    layout->addWidget(searchWidget);

    setLayout(layout);
    update();
}

void Window::update() {
    volunteersList->clear();
    unassignedList->clear();

    std::vector<Volunteer> volunteers = service.getVolunteers();
    sort(volunteers.begin(), volunteers.end(), [](Volunteer a, Volunteer b) {
        return a.name < b.name;
    });

    for (Volunteer volunteer : volunteers) {
        if (volunteer.department == department.name) {
            volunteersList->addItem(QString::fromStdString(volunteer.name));
        } 
        else if (volunteer.department == "") {
            unassignedList->addItem(QString::fromStdString(volunteer.name));
        }
    }
}

void Window::connectSignals() {
    QObject::connect(addVolunteerButton, &QPushButton::clicked, this, &Window::addVolunteer);
    QObject::connect(assignVolunteerButton, &QPushButton::clicked, this, &Window::assignVolunteer);
    QObject::connect(searchButton, &QPushButton::clicked, this, &Window::search);
}

void Window::addVolunteer() {
    std::string name = nameEdit->text().toStdString();
    std::string email = emailEdit->text().toStdString();
    std::string interestsString = interestsEdit->text().toStdString();

    std::vector<std::string> interests;
    std::istringstream iss(interestsString);
    std::string interest;
    while (std::getline(iss, interest, ',')) {
        interests.push_back(interest);
    }

    Volunteer volunteer{name, email, interests, ""};
    service.addVolunteer(volunteer);
    update();
}

void Window::assignVolunteer() {
    int index = unassignedList->currentRow();
    if (index == -1) {
        return;
    }

    std::string volunteerName = unassignedList->currentItem()->text().toStdString();
    service.assignVolunteer(volunteerName, department.name);
    update();
}

void Window::search() {
    std::vector<Volunteer> suitableVolunteers = service.getMostSuitableVolunteers(department.name);

    if (suitableVolunteers.empty()) {
        response->setText("No suitable volunteers found.");
    } 
    else {
        std::string responseString = "Suitable volunteers: ";
        for (int i = 0; i < 3 && i < suitableVolunteers.size(); i++) {
            responseString += suitableVolunteers[i].name + ", ";
        }
        responseString.pop_back();
        responseString.pop_back();
        response->setText(QString::fromStdString(responseString));
    }
}