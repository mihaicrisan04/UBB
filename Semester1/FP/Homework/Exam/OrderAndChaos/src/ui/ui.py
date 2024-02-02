from src.constants import *
from src.domain.board import Board
from src.service.board_service import BoardServiceException


class UI:
    def __init__(self, board_service):
        self.board_service = board_service


    def print_start_menu(self):
        print(START_NEW_GAME_OPTION + '. Start new game')
        print(LOAD_GAME_OPTION + '. Load game')
        print(EXIT_OPTION + '. Exit')


    def print_pause_menu(self):
        print(RESUME_OPTION + '. Resume game')
        print(SAVE_AND_EXIT_OPTION + '. Save and exit')

    
    def read_option(self, message, valid_options):
        while True:
            option = input(message).strip()
            if option in valid_options:
                return option
            print('Invalid option')

    
    def run_game(self, board):
        board.last_move = 'P' if board.last_move is None else board.last_move
        while True:
            print('Player' if board.last_move == 'P' else 'Computer')
            print(board)
            print()

            # check if game is over by player win
            if self.board_service.is_winner(board):
                print('Game over. Player wins')
                print(board)
                self.board_service.delete_board(board)
                self.start()
                break

            # check if game is over by computer win
            if self.board_service.is_board_full(board):
                print('Game over. Computer wins')
                print(board)
                self.board_service.delete_board(board)
                self.start()
                break


            if board.last_move == 'C': # player move
                
                option = self.read_option('Option(ex: A1 for move or 0 of pause): ', BOARD_OPTIONS)

                if option == PAUSE_OPTION:
                    self.print_pause_menu()

                    pause_option = self.read_option('Choose an option: ', [RESUME_OPTION, SAVE_AND_EXIT_OPTION])

                    if pause_option == RESUME_OPTION:
                        continue

                    elif pause_option == SAVE_AND_EXIT_OPTION:
                        self.board_service.save_game()
                        break

                else: # player move
                    i, j = ord(option[0].upper()) - ord('A'), int(option[1]) - 1

                    if board.board[i][j] != 0:
                        print('Invalid move')
                        continue

                    piece = self.read_option('Choose a piece(X/O): ', ['X', 'O'])
                    board.place(i, j, piece)
                    board.last_move = 'P'

            elif board.last_move == 'P': # computer move
                i, j, piece = self.board_service.get_best_move(board)
                board.place(i, j, piece)
                board.last_move = 'C'


            
    def start_new_game(self):
        board = self.board_service.create_board()
        self.run_game(board)


    def load_game(self):
        try:
            boards = self.board_service.get_boards()
        except BoardServiceException as bse:
            print(bse)
            
        if not boards:
            print('No saved games')
            return

        for board in boards:
            print(board)
            print()

        # pick a board to resume
        option = self.read_option('Choose a board to resume: ', [str(i) for i in range(1, len(boards) + 1)])
        board = boards[int(option) - 1]
        self.run_game(board)


    def start(self):
        while True:
            self.print_start_menu()

            option = self.read_option('Choose an option: ', [START_NEW_GAME_OPTION, LOAD_GAME_OPTION, EXIT_OPTION])

            if option == START_NEW_GAME_OPTION:
                self.start_new_game()

            elif option == LOAD_GAME_OPTION:
                self.load_game()

            elif option == EXIT_OPTION:
                break

            else:
                print('Invalid option')
