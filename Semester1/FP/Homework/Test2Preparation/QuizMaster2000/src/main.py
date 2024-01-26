from src.repository.repository_questions import QuestionRepository
from src.services.service_questions import QuestionService
from src.ui.ui import UI


def main():
    repo = QuestionRepository('master_questions.txt')
    service = QuestionService(repo)
    ui = UI(service)

    ui.run()



if __name__ == '__main__':
    main()