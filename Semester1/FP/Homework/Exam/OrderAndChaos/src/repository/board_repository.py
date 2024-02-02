from src.domain.board import Board
from src.constants import SIZE


class BoardRepositoryException(Exception):
    def __init__(self, message):
        super().__init__(message)


class BoardRepository:
    def __init__(self, file_name):
        self.file_path = __file__.replace('repository/board_repository.py', 'data/' + file_name)
        self.boards = []
        self.load()


    def get_boards(self) -> list:
        return self.boards


    def load(self):
        try:
            with open(self.file_path, 'r') as f:
                lines = f.readlines()
                boards_count = len(lines) // 7

                for k in range(boards_count):
                    last_move = lines[k * 7][0].strip()
                    last_move = None if last_move == 'N' else last_move
                    board = [[0 for _ in range(SIZE)] for _ in range(SIZE)]
                    for i in range(SIZE):
                        for j in range(SIZE):
                            board[i][j] = lines[k * 7 + 1 + i][j]
                            board[i][j] = 0 if board[i][j] == '0' else board[i][j]

                    self.boards.append(Board(k+1, board, last_move))

        except FileNotFoundError:
            raise BoardRepositoryException('Invalid file name')


    def save(self):
        try:
            with open(self.file_path, 'w') as f:
                for board in self.boards:
                    f.write(repr(board))

        except FileNotFoundError:
            raise BoardRepositoryException('Invalid file name')


    def add(self, board: Board):
        self.boards.append(board)
        self.save()

    
    def create_board(self) -> Board:
        board = Board(len(self.boards) + 1)
        self.add(board)
        return board


    def delete_board(self, board: Board):
        self.boards.remove(board)
        self.save()