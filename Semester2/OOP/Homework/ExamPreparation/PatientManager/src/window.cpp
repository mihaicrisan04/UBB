#include "window.hpp"

#include <iostream>
#include <algorithm>
#include <QBrush>
#include <QMessageBox>
#include <QDate>

Window::Window(Session &session, Doctor doctor, QWidget* parent) : session(session), doctor(doctor), QWidget(parent) {
    session.addObserver(this);
    initGUI();
    connectSignals();
}

Window::~Window() {
    // session.removeObserver(this);
}

void Window::initGUI() {
    setWindowTitle(QString::fromStdString(doctor.name));

    mainWidget = new QWidget();
    mainLayout = new QVBoxLayout(mainWidget);

    // Patients list
    patientsList = new QListWidget();

    myPatientsCheckBox = new QCheckBox("My patients");

    addPatientWidget = new QWidget();
    addPatientLayout = new QHBoxLayout(addPatientWidget);

    nameLineEdit = new QLineEdit();
    nameLineEdit->setPlaceholderText("Name");
    diagnosisLineEdit = new QLineEdit();
    diagnosisLineEdit->setPlaceholderText("Diagnosis");
    specialtyLineEdit = new QLineEdit();
    specialtyLineEdit->setPlaceholderText("Specialty");
    doctorLineEdit = new QLineEdit();
    doctorLineEdit->setPlaceholderText("Doctor");
    dateLineEdit = new QLineEdit();
    dateLineEdit->setPlaceholderText("Date");
    addPatientButton = new QPushButton("Add patient");

    addPatientLayout->addWidget(nameLineEdit);
    addPatientLayout->addWidget(diagnosisLineEdit);
    addPatientLayout->addWidget(specialtyLineEdit);
    addPatientLayout->addWidget(doctorLineEdit);
    addPatientLayout->addWidget(dateLineEdit);
    addPatientLayout->addWidget(addPatientButton);

    updatePatientWidget = new QWidget();
    updatePatientLayout = new QHBoxLayout(updatePatientWidget);
    diagnosisLineEdit = new QLineEdit();
    diagnosisLineEdit->setPlaceholderText("New Diagnosis");
    specialtyLineEdit = new QLineEdit();
    specialtyLineEdit->setPlaceholderText("New Specialty");
    updatePatientButton = new QPushButton("Update patient");
    updatePatientButton->setEnabled(false);

    updatePatientLayout->addWidget(diagnosisLineEdit);
    updatePatientLayout->addWidget(specialtyLineEdit);
    updatePatientLayout->addWidget(updatePatientButton);

    // Add widgets to layout
    mainLayout->addWidget(patientsList);
    mainLayout->addWidget(myPatientsCheckBox);
    mainLayout->addWidget(addPatientWidget);
    mainLayout->addWidget(updatePatientWidget);

    update();

    setLayout(mainLayout);
}

void Window::connectSignals() {
    connect(myPatientsCheckBox, &QCheckBox::stateChanged, this, &Window::showMyPatients);
    connect(addPatientButton, &QPushButton::clicked, this, &Window::addPatient);
    connect(patientsList, &QListWidget::itemClicked, this, [this](QListWidgetItem *item) {
        Patient patient = session.getPatientByName(item->text().toStdString());
        if (patient.doctor != doctor.name && patient.diagnosis != "undiagnosed") {
            return;
        }
        
        updatePatientButton->setEnabled(true);
        diagnosisLineEdit->setText(QString::fromStdString(patient.diagnosis));
        specialtyLineEdit->setText(QString::fromStdString(patient.specialty));
    });
    connect(updatePatientButton, &QPushButton::clicked, this, &Window::updatePatient);
}   

void Window::update() {
    patientsList->clear();

    std::vector<Patient> patients = session.getPatientsBy(doctor.specialty);
    sort(patients.begin(), patients.end(), [](const Patient &a, const Patient &b) {
        return a.date < b.date;
    });

    for (Patient patient : patients) {
        if (myPatientsCheckBox->isChecked() && patient.doctor != doctor.name) {
            continue;
        }
        QListWidgetItem *item = new QListWidgetItem(QString::fromStdString(patient.name));
        if (patient.doctor == doctor.name) {
            item->setBackground(QBrush(Qt::green));
        }
        patientsList->addItem(item);
    }
}

void Window::showMyPatients() {
    update();
}

void Window::addPatient() {
    std::string name = nameLineEdit->text().toStdString();
    std::string diagnosis = diagnosisLineEdit->text().toStdString();
    std::string specialty = specialtyLineEdit->text().toStdString();
    std::string doctor = doctorLineEdit->text().toStdString();
    std::string date = dateLineEdit->text().toStdString();

    QDate currentDate = QDate::currentDate();
    QString dateString = currentDate.toString("yyyy.MM.dd");

    if (name.empty() || date < dateString.toStdString()) {
        QMessageBox::warning(this, "Warning", "Name or date are invalid!");
        return;
    }

    nameLineEdit->clear();
    diagnosisLineEdit->clear();
    specialtyLineEdit->clear();
    doctorLineEdit->clear();
    dateLineEdit->clear();

    Patient patient(name, diagnosis, specialty, doctor, date);
    session.addPatient(patient);
}

void Window::updatePatient() {
    std::string name = patientsList->currentItem()->text().toStdString();
    std::string diagnosis = diagnosisLineEdit->text().toStdString();
    std::string specialty = specialtyLineEdit->text().toStdString();

    if (diagnosis.empty() || specialty.empty() || diagnosis == "undiagnosed") {
        QMessageBox::warning(this, "Warning", "Diagnosis or specialty are invalid!");
        return;
    }

    session.updatePatient(name, specialty, diagnosis);
    updatePatientButton->setEnabled(false);
    diagnosisLineEdit->clear();
    specialtyLineEdit->clear();
}