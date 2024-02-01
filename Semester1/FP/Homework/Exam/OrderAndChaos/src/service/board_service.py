from src.repository.board_repository import BoardRepositoryException
from src.constants import *


class BoardServiceException(Exception):
    def __init__(self, message):
        super().__init__(message)   


class BoardService:
    def __init__(self, repository):
        self.repository = repository

    