from src.repository.repository_exception import RepositoryException
from src.service.service_exception import ServiceException


class FlightService:
    def __init__(self, flight_repository):
        self.__flight_repository = flight_repository

    def get_all(self) -> list:
        return self.__flight_repository.get_all()

    def add_flight(self, flight_id, departure_city, departure_time, arrival_city, arrival_time):
        try:
            self.__flight_repository.add_flight(flight_id, departure_city, departure_time, arrival_city, arrival_time)
        except RepositoryException as re:
            raise ServiceException(re)

    def delete_flight(self, flight_id):
        try: 
            flight = self.__flight_repository.delete_flight(flight_id)
            return flight
        except RepositoryException as re:
            raise ServiceException(re)

    def get_airports(self) -> list:
        flights = self.__flight_repository.get_all()
        airports = {}
        for flight in flights:
            if flight.departure_city in airports:
                airports[flight.departure_city] += 1
            else:
                airports[flight.departure_city] = 1
            
            if flight.arrival_city in airports:
                airports[flight.arrival_city] += 1
            else:
                airports[flight.arrival_city] = 1

        # convert to a list (airport, freq)
        airports = [(key, value) for key, value in airports.items()]

        if len(airports) == 0:
            raise ServiceException("There is no activity")

        airports = sorted(airports, key=lambda x: x[1], reverse=True)
        return airports
    

    def get_time_intervals(self) -> list:
        flights = self.__flight_repository.get_all()
        time = [False for _ in range(24 * 60 + 1)]

        for flight in flights:
            departure = flight.departure_time.split(':')
            h, m = int(departure[0]), int(departure[1])
            start = h * 60 + m

            arrival = flight.arrival_time.split(':')
            h, m = int(arrival[0]), int(arrival[1])
            end = h * 60 + m

            for i in range(start, end):
                time[i] = True

        intervals = []

        i = 0
        while i < 24 * 60:
            if time[i] == False:
                j = i
                while time[j] == False and j < 24 * 60:
                    j += 1

                start = str(i//60) + ':' + str(i%60)
                end = str(j//60) + ':' + str(j%60)
                duration = str((j-i)//60) + ' hours ' + str((j-i)%60) + ' minutes'
                duration_int = (j-i)//60 + (j-i)%60

                intervals.append((start, end, duration, duration_int))

                i = j
            else:
                i += 1

        if len(intervals) == 0:
            raise ServiceException("There is no free time in the current day.")

        intervals = sorted(intervals, key=lambda x: x[3], reverse=True)

        return intervals

    def __get_sorted_flights(self) -> list:
        flights = self.__flight_repository.get_all()
        return sorted(flights, key=lambda x: (x.departure_time, x.arrival_time))

    def maximum_flights_schedule(self) -> list:
        sorted_flights = self.__get_sorted_flights()

        max_flights = []
        current_flight = None   

        for flight in sorted_flights:
            if current_flight is None or flight.departure_time >= current_flight.arrival_time:
                max_flights.append(flight)
                current_flight = flight

        return max_flights


