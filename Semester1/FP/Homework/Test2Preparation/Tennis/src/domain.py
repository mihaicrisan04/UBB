


class Player:
    def __init__(self, id, name, strength):
        self.id = id
        self.name = name
        self.strength = strength

    def __str__(self) -> str:
        return f'{self.id},{self.name},{self.strength}'

    def __eq__(self, other) -> str:
        if not isinstance(other, Player):
            return False
        return self.id == other.id
