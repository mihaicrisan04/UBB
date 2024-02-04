from src.service.game_service import GameServiceException


class UI:
    def __init__(self, game_service):
        self.game_service = game_service


    def read_command(self):
        return input('>>>').strip()


    def translate_coordinates(self, coordinates_str):
        i, j = coordinates_str[0], coordinates_str[1]
        if not i.isalpha() or not j.isdigit():
            raise ValueError
        i = ord(i.upper()) - 65
        j = int(j) - 1
        return i, j


    def start(self):

        while True:
            self.game_service.update_board()
            print(self.game_service.get_board_str())

            command = self.read_command()

            if 'warp' in command:
                coordinates_str = command.replace('warp', '').strip()

                try:
                    i, j = self.translate_coordinates(coordinates_str)
                    status = self.game_service.warp(i, j)

                    if status == False:
                        print('You have been destroyed by the Blingon ships.')
                        break
                    else:
                        print('You have successfully warped.')

                except GameServiceException as gse:
                    print(gse)
                    continue
                except ValueError:
                    print('Invalid coordinates.')
                    continue


            elif 'fire' in command:
                coordinates_str = command.replace('fire', '').strip()
                try:
                    i, j = self.translate_coordinates(coordinates_str)
                    status = self.game_service.fire(i, j)

                    if status == False:
                        print('Missed.')
                    else:
                        print('Hit. Blingon ship destroyed.')

                except GameServiceException as gse:
                    print(gse)
                    continue
                except ValueError:
                    print('Invalid coordinates.')
                    continue

            elif 'cheat' in command:
                print(self.game_service.cheat())

            elif 'exit' in command:
                break

            else:
                print('Invalid command.')
                continue

            if self.game_service.check_win():
                print('You have destroyed all the Blingon ships. You win!')
                break

