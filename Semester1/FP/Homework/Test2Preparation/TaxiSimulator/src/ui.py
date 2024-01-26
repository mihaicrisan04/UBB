from service import ServiceException


class UI:   
    def __init__(self, service):
        self.service = service

        # consts

    @staticmethod
    def print_menu():
        print()
        print('1. Add a ride')
        print('2. Simulate a ride')
        print('3. Exit')

    @staticmethod
    def get_num_of_taxis() -> int:
        while True:
            n = input('Enter the number of taxis to start: ')
            try:
                n = int(n)
                return n
            except ValueError:
                print('Enter an integer')

    @staticmethod
    def get_command() -> str:
        return input('>>>').strip()


    def run(self):
        n = UI.get_num_of_taxis()

        self.service.generate_taxis(n)

        while True:
            UI.print_menu()

            command = UI.get_command()

            if command == '1':
                start = input('Enter the start coordinates(x,y): ').strip()
                end = input('Enter the end coordinates(x,y): ').strip()

                try:
                    self.service.calculate_ride(start, end)
                except ServiceException as se:
                    print(se)

            elif command == '2':
                self.service.simulate_ride()

            elif command == '3':
                break

            else:
                print('Invalid message')


            taxis = self.service.ordered_taxis()
            for taxi in taxis:
                print(taxi)

            


