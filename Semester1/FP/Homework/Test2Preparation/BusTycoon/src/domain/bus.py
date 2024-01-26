

class Bus:
    def __init__(self, bus_id: int, route_code: int, model: str, time_used: int):
        self.__bus_id = bus_id
        self.__route_code = route_code
        self.__model = model
        self.__time_used = time_used

    def __eq__(self, other) ->bool:
        if not isinstance(other, Bus):
            return False
        return self.__bus_id == other.bus_id

    def __str__(self) -> str:
        return f'bus id: {self.__bus_id}, route code: {self.__route_code}, model: {self.__model}, time_used: {self.time_used}'

    @property
    def bus_id(self) -> int:
        return self.__bus_id

    @property
    def route_code(self) -> int:
        return self.__route_code

    @property
    def model(self) -> str:
        return self.__model

    @property
    def time_used(self) -> int:
        return self.__time_used

    @time_used.setter
    def time_used(self, value):
        self.__time_used = value