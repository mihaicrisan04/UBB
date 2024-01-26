from src.repository.repository import Repository
from src.repository.repository_exception import RepositoryException
from src.domain.route import Route


class RouteRepository(Repository):
    def __init__(self, file_name):
        super().__init__()
        self.__file_path = 'src/repository/' + file_name
        self.read_data()


    def read_data(self):
        try:
            with open(self.__file_path, 'r') as file:
                for line in file:
                    parameters = line.strip().split(',')
                    self.add_route(*parameters)
        except IOError:
            raise RepositoryException('Could not read file: ' + self.__file_path)


    def save_data(self):
        try:
            with open(self.__file_path, 'w') as file:
                for route in self.data.values():
                    string = str(route.route_code) + ','
                    string += str(route.length) + '\n'
                    file.write(string)
        except IOError:
            raise RepositoryException('Could not save to file: ' + self.__file_path)

    
    def add_route(self, route_code: str, length: str):
        try:
            route_code = int(route_code)
        except ValueError:
            raise RepositoryException(f'Route code must be an integer: {route_code}')

        try:
            length = int(length)
        except ValueError:
            raise RepositoryException(f'Length must be an integer: {length}')

        route = Route(route_code, length)

        if route_code in self.data:
            raise RepositoryException(f'Route with id {route_code} already in the repository')
        else:
            self.data[route_code] = route
            self.save_data()

    
    def delete_route(route_code: str) -> object:
        try: 
            route_code = int(route_code)
        except ValueError:
            raise RepositoryException(f'Route code must be an integer: {route_code}')

        if route_code not in self.data:
            raise RepositoryException(f"Route with route code: {route_code} not found in repository")
        else:
            route = self.data.pop(route_code)
            self.save_data()
            return route