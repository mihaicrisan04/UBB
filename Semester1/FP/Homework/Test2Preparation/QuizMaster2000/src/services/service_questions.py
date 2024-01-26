from src.repository.repository_exception import RepositoryException
from src.services.service_exception import ServiceException
from src.domain.question import Question
import random 


class QuestionService:
    def __init__(self, question_repo):
        self.question_repo = question_repo
        self.data_path = './src/data/'

    def get_all(self) -> list:
        return self.question_repo.get_all()

    def add_question(self, id: str, text: str, choice_a: str, choice_b: str, choice_c: str, correct_choice: str, difficulty: str):
        try:
            id = int(id)
        except ValueError:
            raise ServiceException(f'Id must be an integer: {id}')

        try:
            self.question_repo.add_question(id, text, choice_a, choice_b, choice_c, correct_choice, difficulty)
        except RepositoryException as re:
            raise ServiceException(re)

        for question in self.question_repo.get_all():
            print(question)

    def __get_quiz_questions(self, num_of_questions: int, difficulty: str) -> list:
        available_questions = self.get_all()
        quiz_questions = []

        # get neccessary question of specified dificulty
        remaingin_to_add = num_of_questions // 2
        num_of_questions -= remaingin_to_add
        while remaingin_to_add != 0:
            random_index = random.randint(0, len(available_questions) - 1)
            if available_questions[random_index].difficulty == difficulty:
                quiz_questions.append(available_questions[random_index])
                available_questions.pop(random_index)
                remaingin_to_add -= 1

        # add the other remaining random questions
        remaingin_to_add = num_of_questions
        while remaingin_to_add != 0:
            random_index = random.randint(0, len(available_questions) - 1)
            quiz_questions.append(available_questions[random_index])
            available_questions.pop(random_index)
            remaingin_to_add -= 1

        return quiz_questions


    def create_quiz(self, difficulty: str, num_of_questions: str, file_name: str):
        # check if the commands are valid
        try:
            num_of_questions = int(num_of_questions)
        except ValueError:
            raise ServiceException(f'The number of questions must be an integer: {num_of_questions}')
        
        if difficulty not in ['easy', 'medium', 'hard']:
            raise ServiceException(f'Invalid difficulty: {difficulty}')

        questions = self.__get_quiz_questions(num_of_questions, difficulty)
        for question in questions:
            print(question)

        file_path = self.data_path + file_name
        with open(file_path, 'w') as file:
            for question in questions:
                string = str(question) + '\n'
                file.write(string)

    def get_quiz(self, file_name: str) -> list:
        file_path = self.data_path + file_name
        questions = []
        try: 
            with open(file_path, 'r') as file:
                for line in file:
                    parameters = line.strip().split(';')
                    parameters[0] = int(parameters[0])  # convert the id to int
                    question = Question(*parameters)
                    questions.append(question)
        except IOError:
            raise ServiceException(f'Error reading the file: {file_name}, this quiz does not exist')
        
        return questions
        

        
