from src.repository.repository_exception import RepositoryException
from src.domain.question import Question


class QuestionRepository:
    def __init__(self, file_name):
        self.file_path = './src/data/' + file_name
        self.data = {}
        self.read_data()

    def read_data(self):
        try:
            with open(self.file_path, 'r') as file:
                for line in file:
                    parameters = line.strip().split(';')
                    self.add_question(*parameters)
        except IOError:
            raise RepositoryException(f'Could not read the file: {self.file_path}')

    def save_data(self):
        try:
            with open(self.file_path, 'w') as file:
                for question in self.data.values():
                    string = str(question) + '\n'
                    file.write(string)
        except IOError:
            raise RepositoryException(f'Could not save to file: {self.file_path}')

    def add_question(self, id: int, text: str, choice_a: str , choice_b: str, choice_c: str, correct_choice: str, difficulty: str):
        if not isinstance(id, int):
            try:
                id = int(id)
            except ValueError:
                raise RepositoryException(f'Id must be an integer: {id}')

        if id in self.data:
            raise RepositoryException(f'Question with Id {id} already in repository')

        question = Question(id, text, choice_a, choice_b, choice_c, correct_choice, difficulty)
        self.data[id] = question
        self.save_data()

    def delete_question(self, id: str) -> object:
        try:
            id = int(id)
        except ValueError:
            raise RepositoryException(f'Id must be an integer: {id}')

        if id not in self.data:
            raise RepositoryException(f'Question with Id {id} not in repository')


        question = self.data.pop(id)
        self.save_data()
        return question

    def get_all(self) -> list:
        return list(self.data.values())
