import random


class GameService():
    def __init__(self, board, snake):
        self.board = board
        self.snake = snake
        self.score = 0
        self.food = (0, 0)
        self.generate_food()


    def generate_food(self):
        while True:
            x = random.randint(0, self.board.size - 1)
            y = random.randint(0, self.board.size - 1)
            if self.board.board[x][y] == 0:
                self.food = (x, y)
                self.board.board[x][y] = 3
                break

    
    def update_board(self):
        self.board.update(self.snake, self.food)


    def get_board(self):
        return self.board

    
    def is_valid_move(self, direction):
        head = self.snake.get_head()
        if direction == 'w':
            new_head = (head[0] - 1, head[1])
        elif direction == 'a':
            new_head = (head[0], head[1] - 1)
        elif direction == 's':
            new_head = (head[0] + 1, head[1])
        elif direction == 'd':
            new_head = (head[0], head[1] + 1)

        if new_head[0] < 0 or new_head[0] >= self.board.size or new_head[1] < 0 or new_head[1] >= self.board.size:
            return False

        if self.board.board[new_head[0]][new_head[1]] == 1:
            return False

        return True

    
    def move_snake(self, direction):
        head = self.snake.get_head()
        if direction == 'w':
            new_head = (head[0] - 1, head[1])
        elif direction == 'a':
            new_head = (head[0], head[1] - 1)
        elif direction == 's':
            new_head = (head[0] + 1, head[1])
        elif direction == 'd':
            new_head = (head[0], head[1] + 1)

        self.snake.move(new_head)
        if new_head == self.food:
            self.score += 1
            self.generate_food()
        else:
            self.snake.remove_tail()
        return True

    
    def has_won(self):
        return self.score == self.board.size * self.board.size - 1

    
    