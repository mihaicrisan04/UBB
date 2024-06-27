#pragma once

#include <string>
#include <vector>
#include <fstream>
#include <sstream>

#include "researcher.hpp"   
#include "idea.hpp"


class Repository {
    private:
        std::vector<Researcher> researchers;
        std::vector<Idea> ideas;    
    public:
        Repository() {
            loadResearchers();
            loadIdeas();
        }

        ~Repository() {
            saveResearchers();
            saveIdeas();
        }

        void loadResearchers() {
            std::ifstream file("../researchers.txt");
            std::string line;

            while (std::getline(file, line)) {
                std::stringstream ss(line);
                std::string name, position;

                std::getline(ss, name, ',');
                std::getline(ss, position, ',');

                researchers.push_back(Researcher(name, position));
            }

            file.close();
        }

        void loadIdeas() {
            std::ifstream file("../ideas.txt");
            std::string line;

            while (std::getline(file, line)) {
                std::stringstream ss(line);
                std::string title, description, status, creator;
                int duration;

                std::getline(ss, title, ',');
                std::getline(ss, description, ',');
                std::getline(ss, status, ',');
                std::getline(ss, creator, ',');
                ss >> duration;

                ideas.push_back(Idea(title, description, status, creator, duration));
            }

            file.close();
        }

        void saveResearchers() {
            std::ofstream file("../researchers.txt");
            for (auto &researcher : researchers) {
                file << researcher.toString() << std::endl;
            }
            file.close();
        }

        void saveIdeas() {
            std::ofstream file("../ideas.txt");
            for (auto &idea : ideas) {
                file << idea.toString() << std::endl;
            }
            file.close();
        }   
    
        std::vector<Researcher> getResearchers() {
            return researchers;
        }

        std::vector<Idea> getIdeas() {
            return ideas;
        }
};
