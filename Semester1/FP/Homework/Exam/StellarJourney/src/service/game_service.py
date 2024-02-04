import random
from src.constants import SIZE, NUM_STARS, NUM_BLINGON_SHIPS


class GameServiceException(Exception):
    def __init__(self, message):
        super().__init__(message)


class GameService:
    def __init__(self, board):
        self.board = board
        self.stars = []
        self.endeavour = None
        self.blingon_ships = []

        self.generate_stars(NUM_STARS)
        self.generate_endeavour()
        self.generate_blingon_ships(NUM_BLINGON_SHIPS)
        self.update_board()

    
    def get_board_str(self) -> str:
        return str(self.board)


    def generate_stars(self, num_stars: int):
        while len(self.stars) < num_stars:
            i = random.randint(0, SIZE)
            j = random.randint(0, SIZE)
            if (i, j) not in self.stars:
                self.stars.append((i, j))


    def generate_endeavour(self):
        while True:
            i = random.randint(0, SIZE)
            j = random.randint(0, SIZE)
            if (i, j) not in self.stars:
                self.endeavour = (i, j)
                break


    def generate_blingon_ships(self, num_blingon_ships):
        while len(self.blingon_ships) > 0:
            self.blingon_ships.pop()

        while len(self.blingon_ships) < num_blingon_ships:
            i = random.randint(0, SIZE)
            j = random.randint(0, SIZE)
            if (i, j) not in self.stars and (i, j) != self.endeavour:
                self.blingon_ships.append((i, j))


    def update_board(self):
        self.board.update_board(self.stars, self.endeavour, self.blingon_ships)


    def valid_warp(self, i: int, j: int) -> bool:
        return 0 <= i < SIZE and 0 <= j < SIZE

    def warp(self, i: int, j: int) -> bool:
        if not self.valid_warp(i, j):
            raise GameServiceException('Invalid warp coordinates.')

        if (i, j) in self.blingon_ships:
            return False

        i_start, j_start = min(self.endeavour[0], i), min(self.endeavour[1], j)
        i_end, j_end = max(self.endeavour[0], i), max(self.endeavour[1], j)

        # check if there are stars in the path
        if i_start == i_end:  # horizontal warp
            for j in range(j_start, j_end + 1):
                if (i, j) in self.stars:
                    raise GameServiceException('Star encountered. Warp failed.')
        elif j_start == j_end:  # vertical warp
            for i in range(i_start, i_end + 1):
                if (i, j) in self.stars:
                    raise GameServiceException('Star encountered. Warp failed.')
        elif abs(i_start - i_end) == abs(j_start - j_end):  # diagonal warp
            for k in range(i_start, i_end + 1):
                if (k, j_start + k - i_start) in self.stars:
                    raise GameServiceException('Star encountered. Warp failed.')
        else:
            raise GameServiceException('Invalid warp coordinates.')

        self.endeavour = (i, j)
        return True


    def valid_fire(self, i: int, j: int) -> bool:
        return abs(i - self.endeavour[0]) <= 1 and abs(j - self.endeavour[1]) <= 1


    def fire(self, i: int, j: int) -> bool:
        if not self.valid_fire(i, j):
            raise GameServiceException('Invalid fire coordinates.')

        if (i, j) in self.blingon_ships:
            self.blingon_ships.remove((i, j))
            self.generate_blingon_ships(len(self.blingon_ships))
            return True

        return False


    def cheat(self) -> str:
        return self.board.cheat()


    def check_win(self) -> bool:
        return len(self.blingon_ships) == 0

    


