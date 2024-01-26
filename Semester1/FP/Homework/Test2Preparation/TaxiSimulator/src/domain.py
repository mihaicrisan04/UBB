

class Taxi:
    def __init__(self, id: int, x: int, y: int, fare:int):
        self.id = id
        self.x = x
        self.y = y
        self.fare = fare

    def __str__(self) -> str:
        return f'id:{self.id}, x:{self.x}, y:{self.y}, fare:{self.fare}'

    

    