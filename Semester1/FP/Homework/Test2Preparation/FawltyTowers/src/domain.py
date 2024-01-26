from datetime import date


class Reservation:
    def __init__(self, id: str, room_number: int, family_name: str, num_of_guests: int, arrival_date: str, departure_date: str):
        self.id = id
        self.room_number = room_number
        self.family_name = family_name
        self.num_of_guests = num_of_guests
        self.arrival_date = Date(arrival_date)
        self.departure_date = Date(departure_date)

    def __str__(self) -> str:
        return f'{self.id},{self.room_number},{self.family_name},{self.num_of_guests},{str(self.arrival_date)},{str(self.departure_date)}'


class Room:
    def __init__(self, room_number: int, room_type: int):
        self.room_number = room_number
        self.room_type = room_type

    def __str__(self) -> str:
        return f'{self.room_number},{self.room_type}'

class Date:
    def __init__(self, date_string: str):
        day, month = date_string.split('.')
        
        self.date_string = date_string
        self.day = int(day)
        self.month = int(month)

        self.date = date(2018, self.month, self.day)

    def __str__(self) -> str:
        return self.date_string

    def __sub__(self, other) -> int:
        if not isinstance(other, Date):
            return None 
        return abs((self.date - other.date).days)

    def __lt__(self, other):
        if not isinstance(other, Date):
            return None
        return self.date < other.date

    @staticmethod
    def collide(arrival1, departure1, arrival2, departure2) -> bool:
        if departure1 < arrival2:
            return False

        if departure2 < arrival1:
            return False

        return True
    


if __name__ == '__main__':
    date1 = Date('20.02')
    date2 = Date('25.02')

    date3 = Date('23.02')
    date4 = Date('28.02')

    print(date1 - date2)
    print(date2 - date1)
    print(date1 < date2)
    print(date2 < date1)

    print(Date.collide(date1, date2, date3, date4))
        
    
    