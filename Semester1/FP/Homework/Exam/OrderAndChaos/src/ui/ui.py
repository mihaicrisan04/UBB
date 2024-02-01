from src.constants import *
from src.domain.board import Board
from src.service.board_service import BoardServiceException


class UI:
    def __init__(self, game_service):
        self.game_service = game_service

    def _print_start_menu(self):
        print(START_NEW_GAME_OPTION + '. Start new game')
        print(LOAD_GAME_OPTION + '. Load game')
        print(EXIT_OPTION + '. Exit')

    def start(self):
        while True:
