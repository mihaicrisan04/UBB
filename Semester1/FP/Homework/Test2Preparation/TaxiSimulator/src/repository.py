from domain import Taxi
import random

class RepositoryException(Exception):
    def __init__(self, message):
        super().__init__(message)


class Repository:
    def __init__(self):
        self.data = {}
        self.index = 0

    def get_all(self) -> list:
        return list(self.data.values())

    def generate_taxis(self, n: int):
        while n > 0:    
            x = random.randint(0, 100)
            y = random.randint(0, 100)

            taxis = self.get_all()

            ok = True
            for taxi in taxis:
                if self.ManhathanDistance(x, y, taxi.x, taxi.y) < 5:
                    ok = False

            if ok == True:
                self.index += 1
                taxi = Taxi(self.index, x, y, 0)
                self.data[self.index] = taxi
                n -= 1


    def ManhathanDistance(self, x1: int, y1 :int, x2: int, y2: int) -> int:
        return abs(x1 - x2) + abs(y1 - y2)

    # def add_taxi(self, )


