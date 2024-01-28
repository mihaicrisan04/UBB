from repository import RepositoryException
from sentence import Sentence
from random import randint


class ServiceException(Exception):
    def __init__(self, message):
        super().__init__(message)


class SentenceService:
    def __init__(self, repository):
        self.repository = repository    


    def get_all_sentences(self) -> list:
        return self.repository.sentences


    def __validate_sentence(self, string: str) -> None:
        if string == "":
            raise ServiceException("Sentence cannot be empty!")

        for letter in string:
            if not letter.isalpha() and letter != " ":
                raise ServiceException("Sentence can only contain letters and spaces!")

        words = string.split(" ")

        for word in words:
            if len(word) < 3:
                raise ServiceException("Word too short!")
        

    def add_sentence(self, sentence_string: str):
        try:
            self.__validate_sentence(sentence_string)
        except ServiceException as se:
            raise ServiceException(str(se))

        sentence = Sentence.from_string(sentence_string)
        try:
            self.repository.add_sentence(sentence)
        except RepositoryException as re:
            raise ServiceException(str(re))


    def get_random_sentence(self) -> Sentence:
        if len(self.repository.sentences) == 0:
            raise ServiceException("No sentences in repository!")

        index = randint(0, len(self.repository.sentences) - 1)

        return self.repository.sentences[index]