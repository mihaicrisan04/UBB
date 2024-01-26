from src.service.service_exception import ServiceException

class UI:
    def __init__(self, flight_service):
        self.__flight_service = flight_service

        # Magic Numbers
        self.ADD_FLIGHT_OPTION = '1'
        self.DELETE_FLIGHT_OPTION = '2'
        self.AIRPORTS_LIST_OPTION = '3'
        self.TIME_INTERVALS_OPTION = '4'
        self.MAXIMUM_FLIGHTS_OPTION = '5'
        self.EXIT_OPTION = '0'

    def __print_menu(self):
        print()
        print(self.ADD_FLIGHT_OPTION + '. Add flight')
        print(self.DELETE_FLIGHT_OPTION + '. Delete flight')
        print(self.AIRPORTS_LIST_OPTION + '. List the airports, in decreasing order of activity')
        print(self.TIME_INTERVALS_OPTION + '. List the time intervals during which no flights are taking place, in decreasing order of length')
        print(self.MAXIMUM_FLIGHTS_OPTION + '. Maximum number of flights that can proceed as planned')
        print(self.EXIT_OPTION + '. Exit')

    def __read_command(self) -> str:
        return input('>>>')

    def run(self):
        while True:
            self.__print_menu()

            command = self.__read_command()

            if command == self.ADD_FLIGHT_OPTION:
                flight_id = input('Enter flight id: ')
                departure_city = input('Enter the departure city: ')
                departure_time = input('Enter the departure time: ')
                arrival_city = input('Enter the arrival_city: ')
                arrival_time = input('Enter the arrival time: ')

                try: 
                    self.__flight_service.add_flight(flight_id, departure_city, departure_time, arrival_city, arrival_time)
                except ServiceException as se:
                    print(se)

            elif command == self.DELETE_FLIGHT_OPTION:
                print('Current fligths:')
                flights = self.__flight_service.get_all()
                for flight in flights:
                    print(flight)
                
                flight_id = input('Enter the flight id:')

                try:
                    flight = self.__flight_service.delete_flight(flight_id)
                    print('Successfully removed flight: ' + str(flight))
                except ServiceException as se:
                    print(se)


            elif command == self.AIRPORTS_LIST_OPTION:
                try:
                    airports = self.__flight_service.get_airports()
                except ServiceException as se:
                    print(se)

                for airport in airports:
                    print(f'Airport: {airport[0]}; Activity: {airport[1]}')

            elif command == self.TIME_INTERVALS_OPTION:
                try:
                    time_intervals = self.__flight_service.get_time_intervals()
                except ServiceException as se:
                    print(se)

                for interval in time_intervals:
                    print(f'{interval[0]}-{interval[1]}, duration: {interval[2]}')

            elif command == self.MAXIMUM_FLIGHTS_OPTION:
                try: 
                    flights = self.__flight_service.maximum_flights_schedule()
                except ServiceException as se:
                    print(se)

                for flight in flights:
                    string = flight.departure_time + ' | '
                    string += flight.arrival_time + ' | '
                    string += flight.flight_id + ' | '
                    string += flight.departure_city + ' - '
                    string += flight.arrival_city
                    print(string)
            
            elif command == self.EXIT_OPTION:
                break

            else:
                print('Invalid command')

        print('Aplication terminated successfully')