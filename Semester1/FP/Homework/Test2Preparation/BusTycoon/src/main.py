from src.repository.repository_buses import BusRepository
from src.repository.repository_routes import RouteRepository
from src.service.service_buses import BusService
from src.service.service_routes import RouteService
from src.ui.ui import UI


def main():
    bus_repository = BusRepository('buses.txt')
    route_repository = RouteRepository('routes.txt')
    bus_service = BusService(bus_repository)
    route_service = RouteService(route_repository)
    ui = UI(bus_service, route_service)

    ui.run()


if __name__ == '__main__':
    main()