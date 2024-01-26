from src.service.service_exception import ServiceException


class UI:
    def __init__(self, bus_service, route_service):
        self.__bus_service = bus_service
        self.__route_service = route_service

        # Magic numbers
        self.__DISPLAY_BUSES_ON_ROUTE_OPTION = '1'
        self.__INCREASE_USAGE_OPTION = '2'
        self.__DISPLAY_BUSSES_BY_KM_DECREASING_OPTION = '3'
        self.__EXIT_OPTION = '0'

    
    def print_menu(self):
        print()
        print(self.__DISPLAY_BUSES_ON_ROUTE_OPTION + '. Display all buses travelling across a certain route')
        print(self.__INCREASE_USAGE_OPTION + '. Increase usage')
        print(self.__DISPLAY_BUSSES_BY_KM_DECREASING_OPTION + '. Display all buses in decreasing order of kilometers travelled')
        print(self.__EXIT_OPTION + '. Exit')
    
    def read_command(self) -> str:
        return input('>>>')

    def run(self):
        while True:
            self.print_menu()

            command = self.read_command()

            if command == self.__DISPLAY_BUSES_ON_ROUTE_OPTION:
                route_code = input('Enter route code: ')

                try:
                    buses_on_route = self.__bus_service.get_buses_on_route(route_code)
                except ServiceException as se:
                    print(se)

                for bus in buses_on_route:
                    print(bus)

            elif command == self.__INCREASE_USAGE_OPTION:
                bus_id = input('Enter the bus id: ')
                route_code = input('Enter the route code: ')

                try:
                    self.__bus_service.increase_usage(bus_id, route_code)
                    print(f'Successfully increase usage for bus with id {bus_id} on route {route_code}')
                except ServiceException as se:
                    print(se)

            elif command == self.__DISPLAY_BUSSES_BY_KM_DECREASING_OPTION:
                routes = self.__route_service.get_routes()

                try:
                    buses_by_km = self.__bus_service.get_buses_by_km(routes)
                except ServiceException as se:
                    print(se)

                for bus, distance in buses_by_km:
                    print(str(bus) + ' -> ' + str(distance) + ' km')
                
            
            elif command == self.__EXIT_OPTION:
                break

            else:
                print('Invalid command')

        print('Application terminated successfully')