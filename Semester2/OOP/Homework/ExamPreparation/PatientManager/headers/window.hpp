#pragma once

#include <QWidget>
#include <QVBoxLayout>
#include <QHBoxLayout>
#include <QListWidget>
#include <QListWidgetItem>
#include <QPushButton>
#include <QLineEdit>
#include <QLabel>
#include <QCheckBox>

#include "session.hpp"


class Window: public QWidget, public Observer {
    private:
        Session &session;
        Doctor doctor;

        QWidget *mainWidget;
        QVBoxLayout *mainLayout;

        QListWidget *patientsList;

        QCheckBox *myPatientsCheckBox;

        QWidget *addPatientWidget;
        QHBoxLayout *addPatientLayout;
        QLineEdit *nameLineEdit;
        QLineEdit *diagnosisLineEdit;
        QLineEdit *specialtyLineEdit;
        QLineEdit *doctorLineEdit;
        QLineEdit *dateLineEdit;
        QPushButton *addPatientButton;

        QWidget *updatePatientWidget;
        QHBoxLayout *updatePatientLayout;
        QLineEdit *updateDiagnosisLineEdit;
        QLineEdit *updateSpecialtyLineEdit;
        QPushButton *updatePatientButton;


    public:
        Window(Session &session, Doctor doctor, QWidget *parent = nullptr);
        ~Window() override;

        void initGUI();
        void connectSignals();

        void update() override;
    
    public slots:
        void addPatient();
        void showMyPatients();
        void updatePatient();
};