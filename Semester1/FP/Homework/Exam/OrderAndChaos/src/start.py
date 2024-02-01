from src.repository.board_repository import BoardRepository
from src.service.board_service import BoardService
from src.ui.ui import UI


def main():
    repository = BoardRepository('boards.txt')
    service = BoardService(repository)
    ui = UI(service)
    ui.start()


if __name__ == '__main__':
    main()