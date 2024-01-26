from domain import Reservation, Room

class RepositoryException(Exception):
    def __init__(self, message):
        super().__init__(message)


class Repository:
    def __init__(self, file_name):
        self.file_name = file_name
        self.data = {}


    def save_data(self):
        try:
            with open(self.file_name, 'w') as file:
                for item in self.data.values():
                    string = str(item) + '\n'
                    file.write(string)
        except IOError:
            raise RepositoryException(f'Error saving too file: {self.file_name}')


    def get_all(self) -> list:
        return list(self.data.values())


class ReservationRepository(Repository):
    def __init__(self, file_name):
        super().__init__(file_name)
        self.file_name = file_name
        self.read_data()


    def read_data(self):
        try:
            with open(self.file_name, 'r') as file:
                for line in file:
                    parameters = line.strip().split(',')
                    self.add_reservation(*parameters)
        except IOError:
            raise RepositoryException(f'Error reading the file: {self.file_name}')


    def add_reservation(self, id: str, room_number: str, family_name: str, num_of_guests: str, arrival_date: str, departure_date: str):
        try:
            num_of_guests = int(num_of_guests)
            room_number = int(room_number)
        except ValueError:
            raise RepositoryException(f'Error converting to integer one of the following: {num_of_guests} {room_type}')

        if id in self.data.values():
            raise RepositoryException(f'Reservation with id-{id} already exists')

        reservation = Reservation(id, room_number, family_name, num_of_guests, arrival_date, departure_date)
        self.data[id] = reservation
        self.save_data()


    def delete_reservation(self, id: str) -> object:
        if id not in self.data:
            raise RepositoryException(f'Reservation with id-{id} does not exist')

        reservation = self.data.pop(id)
        self.save_data()
        return reservation


class RoomRepository(Repository):
    def __init__(self, file_name):
        super().__init__(file_name)
        self.file_name = file_name
        self.read_data()


    def read_data(self):
        try:
            with open(self.file_name, 'r') as file:
                for line in file:
                    parameters = line.strip().split(',')
                    self.add_room(*parameters)
        except IOError:
            raise RepositoryException(f'Error reading the file: {self.file_name}')


    def add_room(self, room_number, room_type):
        try:
            room_number = int(room_number)
            room_type = int(room_type)
        except ValueError:
            raise RepositoryException(f'Error converting to integer one of the following: {room_number} {room_type}')

        if room_number in self.data.values():
            raise RepositoryException(f'Room with number-{room_number} already exists')

        room = Room(room_number, room_type)
        self.data[room_number] = room
        self.save_data()


    def delete_room(self, room_number: str) -> object:
        try:
            room_number = int(room_number)
        except ValueError:
            raise RepositoryException(f'Error converting to int the room_number: {room_number}')

        if room_number not in self.data:
            raise RepositoryException(f'Room with id-{room_number} does not exist')

        room = self.data.pop(room_number)
        self.save_data()
        return room