from src.repository.repository import Repository
from src.repository.repository_exception import RepositoryException
from src.domain.bus import Bus


class BusRepository(Repository):
    def __init__(self, file_name):
        super().__init__()
        self.__file_path = 'src/repository/' + file_name
        self.read_data()
    

    def read_data(self):
        try:
            with open(self.__file_path, 'r') as file:
                for line in file:
                    parameters = line.strip().split(',')
                    self.add_bus(*parameters)
        except IOError:
            raise RepositoryException('Could not read file: ' + self.__file_path)


    def save_data(self):
        try:
            with open(self.__file_path, 'w') as file:
                for bus in self.data.values():
                    string = str(bus.bus_id) + ','
                    string += str(bus.route_code) + ','
                    string += str(bus.model) + ','
                    string += str(bus.time_used) + '\n'
                    file.write(string)
        except IOError:
            raise RepositoryException('Could not save to file: ' + self.__file_path)


    def add_bus(self, bus_id: str, route_code: str, model: str, time_used: str):
        try:
            bus_id = int(bus_id)
        except ValueError:
            raise RepositoryException(f'Bus Id must be an integer: {bus_id}')

        try:
            route_code = int(route_code)
        except ValueError:
            raise RepositoryException(f'Route code must be an integer: {route_code}')

        try:
            time_used = int(time_used)
        except ValueError:
            raise RepositoryException(f'The time used must be an integer: {time_used}')

        bus = Bus(bus_id, route_code, model, time_used)

        if bus_id in self.data:
            raise RepositoryException(f'Bus with id {bus_id} already in the repository')
        else:
            self.data[bus_id] = bus
            self.save_data()
        

    def delete_bus(self, bus_id: str) -> object:
        try:
            bus_id = int(bus)
        except ValueError:
            raise RepositoryException(f'Bus id must be an integer: {bus_id}')

        if bus_id not in self.data:
            raise RepositoryException(f'Bus with id: {bus_id} not in the repository')
        else:
            bus = self.data.pop(bus_id)
            self.save_data()
            return bus
