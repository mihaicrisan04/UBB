import texttable as tt

class Board:
    def __init__(self, size):
        self.size = size
        self.board = [[0 for _ in range(size)] for _ in range(size)]

    
    def __str__(self):
        table = tt.Texttable()
        table.add_row(['#' for _ in range(self.size + 2)])
        for i in range(self.size):
            row = ['#']
            for j in range(self.size):
                if self.board[i][j] == 0:
                    row.append(' ')
                elif self.board[i][j] == 1:
                    row.append('X')
                elif self.board[i][j] == 2:
                    row.append('H')
                elif self.board[i][j] == 3:
                    row.append('F')
            row.append('#')
            table.add_row(row)
        table.add_row(['#' for _ in range(self.size + 2)])
        return table.draw()


    def update(self, snake, food):
        self.board = [[0 for _ in range(self.size)] for _ in range(self.size)]
        for x, y in snake.body:
            self.board[x][y] = 1
        head = snake.get_head()
        self.board[head[0]][head[1]] = 2
        self.board[food[0]][food[1]] = 3