from src.repository.repository_text import FlightRepository
from src.service.service_flight import FlightService
from src.ui.ui import UI


def main():
    flight_repository = FlightRepository('flights.txt')
    flight_service = FlightService(flight_repository)
    ui = UI(flight_service)
    ui.run()


if __name__ == '__main__':
    main()
