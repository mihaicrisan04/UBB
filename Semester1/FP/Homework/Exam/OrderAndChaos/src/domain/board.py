import texttable
from src.constants import SIZE


class Board:
    def __init__(self, id: int, board=None, last_move=None):
        self.id = id
        self.size = SIZE
        self.board = [[0 for _ in range(size)] for _ in range(size)] if board is None else board
        self.last_move = last_move

    def __repr__(self) -> str:
        s = 'N' if self.last_move is None else str(self.last_move)
        s += '\n'
        for i in range(self.size):
            for j in range(self.size):
                s += str(self.board[i][j])
            s += '\n'
        return s

    def __str__(self) -> str:
        t = texttable.Texttable()
        t.add_row([''] + [i for i in range(self.size)])
        for i in range(self.size):
            t.add_row([chr(i + 65)] + self.board[i])
        return t.draw()

    def place(self, row: int, col: int, piece):
        self.board[row][col] = piece



if __name__ == '__main__':
    board = Board(6)
    print(board)
    print(repr(board))
