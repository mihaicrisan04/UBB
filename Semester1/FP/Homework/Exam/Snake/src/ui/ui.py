


class UI:
    def __init__(self, game_service):
        self.game_service = game_service


    def read_direction(self):
        while True:
            direction = input('Enter direction: ').strip().lower()
            if direction in ['w', 'a', 's', 'd']:
                return direction
            else:
                print('Invalid direction')


    def start(self):
        while True:
            print(self.game_service.get_board())

            direction = self.read_direction()

            if not self.game_service.is_valid_move(direction):
                print('Game over')
                break

            self.game_service.move_snake(direction)

            if self.game_service.has_won():
                print('You won')
                break


