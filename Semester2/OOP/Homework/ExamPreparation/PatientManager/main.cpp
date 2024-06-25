#include <iostream>

#include <QApplication>

#include "window.hpp"
#include "chart.hpp"

int main(int argc, char *argv[]) {
    QApplication app(argc, argv);

    Repository repo;
    Session session(repo);

    // std::vector<Doctor> doctors = repo.getDoctors();    
    // for (Doctor &doctor : doctors) {
    //     std::cout << doctor.toString() << std::endl;
    // }
    // std::cout << std::endl;
    // std::vector<Patient> patients = repo.getPatients();
    // for (Patient &patient : patients) {
    //     std::cout << patient.name << " " << patient.diagnosis << " " << patient.specialty << " " << patient.doctor << " " << patient.date << std::endl;
    // }

    for (Doctor &doctor : session.getDoctors()) {
        Window *window = new Window(session, doctor);
        window->show();
    }

    Chart *chart = new Chart(session);
    chart->show();

    return app.exec();
}
