from service import ServiceException


class UI:
    def __init__(self, service):
        self.service = service
    
    @staticmethod
    def print_menu():
        print()
        print('1. Add a new student')
        print('2. Dsiplay all students in decreasing order of their grade')
        print('3. Give bonuses')
        print('4. Display all students whose name include a given string, ordered by name')
        print('5. Exit')

    
    @staticmethod
    def read_command() -> str:
        return input('>>>').strip()


    def run(self):
        while True:
            UI.print_menu()

            command = UI.read_command()

            if command == '1':
                    id = input('Enter the id: ').strip()
                    name = input('Enter the name: ').strip()
                    attendence = input('Enter the attendence: ').strip()
                    grade = input('Enter the grade: ').strip()

                    try:
                        self.service.add_sudent(id, name, attendence, grade)
                    except ServiceException as se:
                        print(se)

            elif command == '2':
                students = self.service.ordered_by_grade()

                for student in students:
                    print(student)

            elif command == '3':
                p = input('Enter the minimum numberof attendences: ').strip()
                b = input('Enter the amount of the bonus: ').strip()

                try:
                    self.service.give_bonuses(p, b)
                except ServiceException as se:
                    print(se)

            elif command == '4':
                name = input('Enther a name to search: ').strip()

                students = self.service.serach_by_name(name)

                if len(students) == 0:
                    print('No students with that name')
                else:
                    for student in students:
                        print(student)
                        
            elif command == '5':
                break

            else:
                print('Invalid command')