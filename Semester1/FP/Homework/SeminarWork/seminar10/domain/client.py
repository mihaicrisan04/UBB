

class Client:
    def __init__(self, id, name):
        slef.__id = id
        self.__name = name

    def __eq__(self, other):
        if not isinstance(other, Client):
            return False
        return self.__id == other.id

    def __str__(self):
        return f"(Id: {self.id}, Name:{self.name})"

    def __repr__(self):
        return f"(Id: {self.id}, Name:{self.name})"

    @property
    def name(self):
        return self.__name

    @property
    def id(self):
        return self.__id