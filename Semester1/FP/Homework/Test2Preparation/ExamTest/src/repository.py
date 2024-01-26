from domain import Student


class RepositoryException(Exception):
    def __init__(message):
        super().__init__(message)


class Repository:
    def __init__(self, file_name):
        self.file_name = file_name
        self.data = {}
        self.read_data()

    
    def read_data(self):
        try:
            with open(self.file_name, 'r') as file:
                for line in file:
                    parameters = [parameter.strip() for parameter in line.strip().split(',')]
                    self.add_sudent(*parameters)
        except IOError:
            raise RepositoryException(f'Error reading the file: {self.file_name}')

    def save_data(self):
        try:
            with open(self.file_name, 'w') as file:
                for student in self.data.values():
                    file.write(str(student) + '\n')
        except IOError:
            raise RepositoryException(f'Error writing to file: {self.file_name}')


    def add_sudent(self, id: str, name:str, attendence: str, grade: str):
        try:
            id = int(id)
            attendence = int(attendence)
            grade = int(grade)
        except ValueError:
            raise RepositoryException('Error converting to in one of the following: id, attendence of grade')

        name_surname = name.split(' ')
        if len(name_surname) != 2:
            raise RepositoryException('The name must be of two words')

        if grade < 0 or grade > 10:
            raise RepositoryException('Invalid grade')

        if id in self.data:
            raise RepositoryException(f'Student with id-{id} already in the repository')

        student = Student(id, name, attendence, grade)
        self.data[id] = student
        self.save_data()

    
    def get_all(self) -> list:
        return list(self.data.values())


    def get_item_by_id(self, id: int):
        return self.data[id] if not None else None