from service import ServiceException


class UI:
    def __init__(self, room_service, reservation_service):
        self.room_service = room_service
        self.reservation_service = reservation_service

        # Magic numbers
        self.CREATE_RESERVATION_OPTION = '1'
        self.DELETE_RESERVATION_OPTION = '2'
        self.LIST_AVAILABLE_ROOMS_OPTION = '3'
        self.MONTHLY_REPORT_OPTION = '4'
        self.WEEKLY_REPORT_OPTION = '5'
        self.EXIT_OPTION = '0'

    def print_menu(self):
        print()
        print(self.CREATE_RESERVATION_OPTION + '. Create a reservation')
        print(self.DELETE_RESERVATION_OPTION + '. Delete a reservation')
        print(self.LIST_AVAILABLE_ROOMS_OPTION + '. Show available rooms')
        print(self.MONTHLY_REPORT_OPTION + '. Monthly report')
        print(self.WEEKLY_REPORT_OPTION + '. Weekly report')
        print(self.EXIT_OPTION + '. Exit')

    def read_command(self) -> str:
        return input('>>>').strip()
    
    def run(self):
        while True:
            rooms = self.room_service.get_all()
            reservations = self.reservation_service.get_all()

            for room in rooms:
                print(room)
            for reservation in reservations:
                print(reservation)

            # --------------------------------------------

            self.print_menu()

            command = self.read_command()

            if command == self.CREATE_RESERVATION_OPTION:
                rooms = self.room_service.get_all()
                
                family_name = input('Enter family name: ').strip()
                room_type = input('Enter the room type: ').strip()
                num_of_guests = input('Enter the number of guests: ').strip()
                arrival_date = input('Enter the arrival date: ').strip()
                departure_date = input('Enter the departure date: ').strip()
                
                try:
                    self.reservation_service.create_reservation(family_name, room_type, num_of_guests, arrival_date, departure_date, rooms)
                    print('Reservation added successfully')
                except ServiceException as se:
                    print(se)

            elif command == self.DELETE_RESERVATION_OPTION:
                pass

            elif command == self.LIST_AVAILABLE_ROOMS_OPTION:
                pass

            elif command == self.MONTHLY_REPORT_OPTION:
                pass

            elif command == self.WEEKLY_REPORT_OPTION:
                pass

            elif command == self.EXIT_OPTION:
                break
            
            else:
                print('Invalid command')

        print('Application terminated successfully')
            

