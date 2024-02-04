from src.domain.board import Board
from src.service.game_service import GameService
from src.ui.ui import UI
from src.constants import SIZE


def main():
    board = Board(SIZE)
    game_service = GameService(board)
    ui = UI(game_service)
    ui.start()


if __name__ == '__main__':
    main()