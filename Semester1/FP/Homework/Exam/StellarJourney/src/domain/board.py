import texttable as tt


class Board:
    def __init__(self, size=8):
        self.size = size
        self.board = [[0 for _ in range(self.size)] for _ in range(self.size)]


    def get_endeavour_coordinates(self):
        for i in range(self.size):
            for j in range(self.size):
                if self.board[i][j] == 2:
                    return i, j
        return None, None


    def __str__(self) -> str:
        i_endeavour, j_endeavour = self.get_endeavour_coordinates()

        table = tt.Texttable()
        table.add_row([i for i in range(self.size + 1)])
        for i in range(self.size):
            line = [chr(i + 65)]
            for j in range(self.size):
                if self.board[i][j] == 0:
                    line.append(' ')
                elif self.board[i][j] == 1:
                    line.append('*')
                elif self.board[i][j] == 2:
                    line.append('E')
                elif self.board[i][j] == 3 and abs(i - i_endeavour) <= 1 and abs(j - j_endeavour) <= 1:
                    line.append('B')
                else:
                    line.append(' ')
            table.add_row(line)
        return table.draw()


    def clear_board(self):
        for i in range(self.size):
            for j in range(self.size):
                self.board[i][j] = 0


    def update_board(self, stars, endeavour, blingon_ships):
        self.clear_board()
        for i in range(self.size):
            for j in range(self.size):
                if (i, j) in stars:
                    self.board[i][j] = 1
                if (i, j) == endeavour:
                    self.board[i][j] = 2
                if (i, j) in blingon_ships:
                    self.board[i][j] = 3 


    def cheat(self) -> str:
        table = tt.Texttable()
        table.add_row([i for i in range(self.size + 1)])
        for i, row in enumerate(self.board):
            line = [chr(i + 65)]
            for col in row:
                if col == 0:
                    line.append(' ')
                if col == 1:
                    line.append('*')
                if col == 2:
                    line.append('E')
                if col == 3:
                    line.append('B')
            table.add_row(line)
        return table.draw()



if __name__ == '__main__':
    b = Board()
    b.update_board([(0, 0), (0, 1), (1, 0), (1, 1)], (2, 2), [(3, 3), (4, 4), (5, 5)])
    print(b)