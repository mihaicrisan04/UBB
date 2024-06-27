#pragma once

#include "subject.hpp"
#include "repository.hpp"


class Session : public Subject {
    private:
        Repository &repo;
    
    public:
        Session(Repository &repo) : repo(repo) {}


        std::vector<Researcher> getResearchers() {
            return repo.getResearchers();
        }   

        std::vector<Idea> getIdeas() {
            return repo.getIdeas();
        }   
};
