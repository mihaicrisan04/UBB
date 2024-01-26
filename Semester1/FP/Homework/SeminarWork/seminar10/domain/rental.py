from datetime import date

class Rental:
    """
    One rental object represents one client who rented one car

    Each rental object has:
        -> id : str
        -> client : Client (who rented the car)
        -> car : Car (car that was rented)
        -> start, end : date (renting interval)
    """
    def __init__(self, id, client, car, start, end):
        self.__id = id
        self.__client = client
        self.__car = car
        self.__start = start
        self.__end = end

    def __eq__(self, other):
        if not isinstance(other, Rental):
            return False
        return self.id == other.id

    def __str__(self):
        return f"Rental Id: {self.id}, Client: {self.client}, Car: {self.car}, Start: {self.start}, End: {self.end}"

    @property
    def id(self):
        return self.__id

    @property
    def client(self):
        return self.__client

    @property
    def car(self):
        return self.__car

    @property
    def start(self):
        return self.__start
    
    @property
    def end(self):
        return self.__end