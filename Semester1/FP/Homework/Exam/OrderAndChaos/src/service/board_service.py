from src.repository.board_repository import BoardRepositoryException
from src.domain.board import Board
from src.constants import *
from random import randint


class BoardServiceException(Exception):
    def __init__(self, message):
        super().__init__(message)   


class BoardService:
    def __init__(self, repository):
        self.repository = repository


    def create_board(self) -> Board:
        return self.repository.create_board()


    def get_boards(self) -> list:
        return self.repository.get_boards()


    def save_game(self):
        self.repository.save()

    
    def is_inside_board(self, i, j) -> bool:
        return 0 <= i < SIZE and 0 <= j < SIZE

    def get_best_move(self, board: Board) -> tuple:
        best_moves = []

        for i in range(0, SIZE-1 - 4):
            for j in range(0, SIZE-1 - 4):
                if board.board[i][j] == 0:
                    continue
                
                # check right
                for k in range(0, 4):
                    if all(board.board[i][j] == board.board[i][j+k] for k in range(4)):
                        if j - 1 >= 0 and board.board[i][j-1] == 0:
                            best_moves.append((i, j-1, 'X' if board.board[i][j] == 'O' else 'O'))
                        if j + 4 < SIZE and board.board[i][j+4] == 0:
                            best_moves.append((i, j+4, 'X' if board.board[i][j] == 'O' else 'O'))

                # check down
                for k in range(0, 4):
                    if all(board.board[i][j] == board.board[i+k][j] for k in range(4)):
                        if i - 1 >= 0 and board.board[i-1][j] == 0:
                            best_moves.append((i-1, j, 'X' if board.board[i][j] == 'O' else 'O'))
                        if i + 4 < SIZE and board.board[i+4][j] == 0:
                            best_moves.append((i+4, j, 'X' if board.board[i][j] == 'O' else 'O'))

                # check diagonal (down-right)
                for k in range(0, 4):
                    if all(board.board[i][j] == board.board[i+k][j+k] for k in range(4)):
                        if i - 1 >= 0 and j - 1 >= 0 and board.board[i-1][j-1] == 0:
                            best_moves.append((i-1, j-1, 'X' if board.board[i][j] == 'O' else 'O'))
                        if i + 4 < SIZE and j + 4 < SIZE and board.board[i+4][j+4] == 0:
                            best_moves.append((i+4, j+4, 'X' if board.board[i][j] == 'O' else 'O'))

                # check diagonal (up-right)
                if i - 4 >= 0 and j + 4 < SIZE:
                    for k in range(0, 4):
                        if all(board.board[i][j] == board.board[i-k][j+k] for k in range(4)):
                            if i + 1 < SIZE and j - 1 >= 0 and board.board[i+1][j-1] == 0:
                                best_moves.append((i+1, j-1, 'X' if board.board[i][j] == 'O' else 'O'))
                            if i - 4 >= 0 and j + 4 < SIZE and board.board[i-4][j+4] == 0:
                                best_moves.append((i-4, j+4, 'X' if board.board[i][j] == 'O' else 'O'))


        if best_moves:
            return best_moves[0]

        # return a random move
        while True:
            i, j = randint(0, SIZE-1), randint(0, SIZE-1)
            if board.board[i][j] == 0:
                print(i, j) 
                piece = 'X' if randint(0, 1) == 0 else 'O'
                return i, j, piece


    def is_winner(self, board: Board) -> bool:
        def check_consecutive(sequence):
            return all(cell == sequence[0] for cell in sequence)

        def check_rows():
            for row in board.board:
                for start_col in range(len(row) - 4):
                    if check_consecutive(row[start_col:start_col + 5]):
                        return True
            return False

        def check_columns():
            for col in range(len(board.board[0])):
                for start_row in range(len(board.board) - 4):
                    if check_consecutive([board.board[start_row + i][col] for i in range(5)]):
                        return True
            return False

        def check_diagonals():
            for start_row in range(len(board.board) - 4):
                for start_col in range(len(board.board[0]) - 4):
                    diagonal = [board.board[start_row + i][start_col + i] for i in range(5)]
                    if check_consecutive(diagonal):
                        return True

                for start_col in range(len(board.board[0]) - 4):
                    diagonal = [board.board[start_row + i][start_col + 4 - i] for i in range(5)]
                    if check_consecutive(diagonal):
                        return True
            return False

        return check_rows() or check_columns() or check_diagonals()


        # for i in range(0, SIZE-1 - 4):
        #     for j in range(0, SIZE-1 - 4):
        #         if board.board[i][j] == 0:
        #             continue
                
        #         # check right
        #         if all(board.board[i][j] == board.board[i][j+k] for k in range(5)):
        #             return True

        #         # check down
        #         if all(board.board[i][j] == board.board[i+k][j] for k in range(5)):
        #             return True

        #         # check diagonal (down-right)
        #         if all(board.board[i][j] == board.board[i+k][j+k] for k in range(5)):
        #             return True

        #         # check diagonal (up-right)
        #         if i - 5 >= 0 and j + 4 < SIZE:
        #             if all(board.board[i][j] == board.board[i-k][j+k] for k in range(5)):
        #                 return True

        # return False

    
    def is_board_full(self, board: Board) -> bool:
        for i in range(SIZE):
            for j in range(SIZE):
                if board.board[i][j] == 0:
                    return False
        return True


    def delete_board(self, board: Board):
        self.repository.delete_board(board)



