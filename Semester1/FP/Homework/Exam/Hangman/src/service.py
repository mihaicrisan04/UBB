from repository import RepositoryException
from sentence import Sentence
from random import randint


class ServiceException(Exception):
    def __init__(self, message):
        super().__init__(message)


class SentenceService:
    def __init__(self, repository):
        self.repository = repository    


    def add_sentence(self, sentence_string: str):
        sentence = Sentence.from_string(sentence_string)
        try:
            self.repository.add_sentence(sentence)
        except RepositoryException as re:
            raise ServiceException(str(re))

        for s in self.repository.sentences:
            print(s)


    def get_random_sentence(self) -> Sentence:
        if len(self.repository.sentences) == 0:
            raise ServiceException("No sentences in repository!")

        index = randint(0, len(self.repository.sentences) - 1)

        return self.repository.sentences[index]