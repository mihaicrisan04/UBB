from src.domain.flight import Flight
from src.repository.repository_exception import RepositoryException


class FlightRepository:
    def __init__(self, file_name):
        self.__file_path = "src/" + file_name
        self.__data = {}
        self.__read_data()

    def __read_data(self):
        try:
            with open(self.__file_path, 'r') as file:
                for line in file:
                    parameters = line.strip().split(',')
                    self.add_flight(*parameters)
        except IOError:
            raise RepositoryException('Could not read file:' + self.__file_path)

    def __save_data(self):
        try:
            with open(self.__file_path, 'w') as file:
                for flight in self.__data.values():
                    string = flight.flight_id + ',' 
                    string += flight.departure_city + ','
                    string += flight.departure_time + ','
                    string += flight.arrival_city + ','
                    string += flight.arrival_time + '\n'
                    file.write(string)
        except IOError:
            raise RepositoryException('Could not write to file:' + self.__file_path)

    def get_all(self) -> list[Flight]:
        return self.__data.values()

    def add_flight(self, flight_id, departure_city, departure_time, arrival_city, arrival_time):
        if flight_id in self.__data:
            raise RepositoryException('Flight with id: ' + flight_id + ' already in the repository')
        self.__data[flight_id] = Flight(flight_id, departure_city, departure_time, arrival_city, arrival_time)
        self.__save_data()

    def delete_flight(self, flight_id) -> Flight:
        if flight_id not in self.__data:
            raise RepositoryException('Flight with id: ' + flight_id + ' doesn\'t exist in the repository')
        
        flight = self.__data.pop(flight_id)
        self.__save_data()
        return flight