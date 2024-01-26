from src.repository.repository_exception import RepositoryException
from src.service.service_exception import ServiceException


class BusService:
    def __init__(self, bus_repository):
        self.__bus_repository = bus_repository

    def get_buses(self) -> list:
        return self.__bus_repository.get_all()

    def get_buses_on_route(self, route_code: str) -> list:
        try: 
            route_code = int(route_code)
        except ValueError:
            raise ServiceException(f'Route must be an integer: {route_code}')

        buses = self.get_buses()

        buses_on_route = []

        for bus in buses:
            if bus.route_code == route_code:  
                buses_on_route.append(bus)

        if len(buses_on_route) == 0:
            raise ServiceException(f'There are no buses on the route: {route_code}')

        return buses_on_route

    
    def increase_usage(self, bus_id: str, route_code: str):
        try: 
            bus_id = int(bus_id)
        except ValueError:
            raise ServiceException(f'Bus id must be an integer: {bus_id}')

        try: 
            route_code = int(route_code)
        except ValueError:
            raise ServiceException(f'Route must be an integer: {route_code}')

        buses = self.get_buses()

        for bus in buses:
            if bus.bus_id == bus_id and bus.route_code == route_code:
                bus.time_used += 1
                break

        self.__bus_repository.save_data()

    def get_buses_by_km(self, routes) -> list:
        buses = self.get_buses()

        buses_by_km = []

        for bus in buses:
            for route in routes:
                if route.route_code == bus.route_code:
                    buses_by_km.append((bus, bus.time_used * route.length))
                    break

        buses_by_km = sorted(buses_by_km, key=lambda x: x[1], reverse=True)

        if len(buses_by_km) == 0:
            raise ServiceException('There are no buses or any routes')

        return buses_by_km
        
