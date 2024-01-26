from repository import RepositoryException
from domain import Date
import random

class ServiceException(Exception):
    def __init__(self, message):
        super().__init__(message)


class Service:
    def __init__(self, repository):
        self.repository = repository

    def get_all(self) -> list:
        return self.repository.get_all()


class RoomService(Service):
    def __init__(self, repository):
        super().__init__(repository)

    
class ReservationService(Service):
    def __init__(self, repository):
        super().__init__(repository)


    def valid_reservation_input(self, family_name, room_type, num_of_guests, arrival_date, departure_date) -> bool:
        try:
            room_type = int(room_type)
            num_of_guests = int(num_of_guests)
        except ValueError:
            raise ServiceException(f'Error converting to int one of the following: {room_type} {num_of_guests}')

        if family_name is None:
            raise ServiceException('A family name must be introduced. Reservation failed')

        if num_of_guests < 1 or num_of_guests > 4:
            raise ServiceException('The number of guests must be between 1 and 4. Reservation failed')
        
        if len(arrival_date) != 5 or len(departure_date) != 5:
            raise ServiceException('Invalid date format')
        
        arr_day, arr_month = arrival_date.split('.')
        dep_day, dep_month = departure_date.split('.')

        arr_day, arr_month = int(arr_day), int(arr_month)
        dep_day, dep_month = int(dep_day), int(dep_month)

        if arr_day < 0 or arr_day > 31 or dep_day < 0 or dep_day > 31:
            raise ServiceException('Invalid date format')
        if arr_month < 0 or arr_month > 12 or dep_month < 0 or dep_month > 12:
            raise ServiceException('Invalid date format')
        
        return True


    def get_reservation_id(self) -> str:
        reservations = self.repository.get_all()
        ids = [reservation.id for reservation in reservations]

        while True:
            new_id = str(random.randint(0, 9)) + str(random.randint(0, 9)) + str(random.randint(0, 9)) + str(random.randint(0, 9))
            if new_id not in ids:
                return new_id


    def get_available_room(self, room_type: str, arrival_date: str, departure_date: str, rooms: list) -> str:
        room_type = int(room_type)

        ok = True
        while ok:
            ok = False
            for i in range(len(rooms)):
                if rooms[i].room_type != room_type:
                    rooms.pop(i)
                    ok = True
                    break

        room_numbers = [room.room_number for room in rooms]

        reservations = self.get_all()

        arrival_date = Date(arrival_date)
        departure_date = Date(departure_date)

        for reservation in reservations:
            if reservation.room_number in room_numbers and Date.collide(arrival_date, departure_date, reservation.arrival_date, reservation.departure_date) == True:  # the room is a valid type of room 
                # delete the room number from the availalbe ones
                room_numbers.remove(reservation.room_number)

        if len(room_numbers) == 0:
            raise ServiceException('No more available rooms of that type for that date interval')
        
        return room_numbers[0]

    def create_reservation(self, family_name, room_type, num_of_guests, arrival_date, departure_date, rooms):
        if self.valid_reservation_input(family_name, room_type, num_of_guests, arrival_date, departure_date):
            new_id = self.get_reservation_id()

            try:
                room_number = self.get_available_room(room_type, arrival_date, departure_date, rooms)
                self.repository.add_reservation(new_id, room_number, family_name, num_of_guests, arrival_date, departure_date)
            except RepositoryException as re:
                raise ServiceException(re)
            
