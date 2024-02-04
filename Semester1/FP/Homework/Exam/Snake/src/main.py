from src.domain.board import Board
from src.domain.snake import Snake
from src.services.service import GameService
from src.ui.ui import UI


def main():
    board = Board(8)
    snake = Snake()
    game_service = GameService(board, snake)
    ui = UI(game_service)
    ui.start()



if __name__ == '__main__':
    main()