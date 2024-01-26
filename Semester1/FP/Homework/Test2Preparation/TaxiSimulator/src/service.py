import random
from repository import RepositoryException


class ServiceException(Exception):
    def __init__(self, message):
        super().__init__(message)


class Service:
    def __init__(self, repo):
        self.repo = repo

    def generate_taxis(self, n: int):
        self.repo.generate_taxis(n)

    def get_all(self) -> list:
        return self.repo.get_all()

    def ordered_taxis(self) -> list:
        taxis = self.get_all()
        return sorted(taxis, key= lambda x: x.fare, reverse=True)

    def assign_taxi(self, x_start, y_start, x_end, y_end) -> object:
        taxis = self.get_all()

        min_dist = 200
        best_taxi = None

        for taxi in taxis:
            dist = self.repo.ManhathanDistance(x_start, y_start, taxi.x, taxi.y)
            if dist < min_dist:
                min_dist = dist
                best_taxi = taxi

        best_taxi.x = x_end
        best_taxi.y = y_end
        best_taxi.fare += self.repo.ManhathanDistance(x_start, y_start, x_end, y_end)
        return best_taxi

    def calculate_ride(self, start: str, end: str):
        try:
            start = start.split(',')
            end = end.split(',')
            x_start, y_start = start[0], start[1]
            x_end, y_end = end[0], end[1]
        except IndexError:
            raise ServiceException('Invalid coordinates input')

        try:
            x_start = int(x_start)
            y_start = int(y_start)
            x_end = int(x_end)
            y_end = int(y_end)
        except ValueError:
            raise ServiceException('Enter integer coordinates')

        # check if position are valid 0 <= pos <= 100

        taxi = self.assign_taxi(x_start, y_start, x_end, y_end)
        

    def simulate_ride(self):
        while True:
            x_start = random.randint(0, 100)
            y_start = random.randint(0, 100)
            x_end = random.randint(0, 100)
            y_end = random.randint(0, 100)

            if self.repo.ManhathanDistance(x_start, y_start, x_end, y_end) >= 10:
                break

        taxi = self.assign_taxi(x_start, y_start, x_end, y_end)
