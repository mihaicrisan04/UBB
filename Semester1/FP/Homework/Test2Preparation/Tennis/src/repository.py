from domain import Player


class RepositoryException(Exception):
    def __init__(self, message):
        super().__init__()


class Repository:
    def __init__(self, file_name):
        self.file_name = file_name
        self.data = {}
        self.read_data()

    def get_all(self) -> list:
        return list(self.data.values())

    def read_data(self):
        try:
            with open(self.file_name, 'r') as file:
                for line in file:
                    parameters = [parameter.strip() for parameter in line.strip().split(',')]
                    self.add_player(*parameters)
        except IOError:
            raise RepositoryException('Error reading the file')

    
    def add_player(self, id: str, name: str, strength: str):
        try:
            id = int(id)
            strength = int(strength)
        except ValueError:
            raise RepositoryException('Error id and strength must be integers')

        player = Player(id, name, strength)
        self.data[id] = player

    def delete_player(self, id: int):
        if id not in self.data:
            raise RepositoryException('Player doesnt exist')
        self.data.pop(id)

    
