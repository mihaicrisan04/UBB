#pragma once

#include "subject.hpp"
#include "repository.hpp"


class Session : public Subject {
    private:
        Repository &repo;
    
    public:
        Session(Repository &repo) : repo(repo) {}

        void addIdea(Idea idea) {
            repo.addIdea(idea);
            notify();
        }

        void updateIdeaStatus(Idea idea, std::string status) {
            repo.updateIdeaStatus(idea, status);
            notify();
        }   

        std::vector<Researcher> getResearchers() {
            return repo.getResearchers();
        }   

        std::vector<Idea> getIdeas() {
            std::vector<Idea> ideas = repo.getIdeas();
            sort(ideas.begin(), ideas.end(), [](const Idea &idea1, const Idea &idea2) {
                return idea1.duration > idea2.duration;
            }); 
            return ideas;
        }   
};
