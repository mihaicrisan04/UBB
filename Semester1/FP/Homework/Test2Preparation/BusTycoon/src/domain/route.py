

class Route:
    def __init__(self, route_code: int, length: int):
        self.__route_code = route_code
        self.__length = length

    @property
    def route_code(self):
        return self.__route_code

    @property
    def length(self):
        return self.__length