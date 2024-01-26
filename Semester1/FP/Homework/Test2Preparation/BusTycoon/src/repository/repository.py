

class Repository:
    def __init__(self):
        self.__data = {}

    def get_all(self) -> list:
        return self.__data.values()

    @property
    def data(self) -> dict:
        return self.__data