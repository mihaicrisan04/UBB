from repository import RepositoryException


class ServiceException(Exception):
    def __init__(message):
        super().__init__(message)


class Service:
    def __init__(self, repository):
        self.repository = repository    

    
    def get_all(self) -> list:
        return self.repository.get_all()


    def add_sudent(self, id: str, name: str, attendence: str, grade: str):
        try:
            self.repository.add_sudent(id, name, attendence, grade)
        except RepositoryException as re:
            raise ServiceException(re)

    def ordered_by_grade(self) -> list:
        students = self.get_all()

        for i in range(len(students) - 1):
            for j in range(i+1, len(students)):
                if students[j].grade > students[i].grade:
                    students[i], students[j] = students[j], students[i]
                elif students[j].grade == students[i].grade:
                    if students[j].name < students[i].name:
                        students[i], students[j] = students[j], students[i]
        
        return students


    def give_bonuses(self, p: str, b: str):
        try:
            p = int(p)
            b = int(b)
        except ValueError:
            raise ServiceException('Invalid format for the attendence or the bonus')

        students = self.repository.get_all()

        for student in students:
            if student.attendence >= p and student.grade + b <= 10:
                student.grade += b


    def serach_by_name(self, name: str) -> list:
        students = self.repository.get_all()

        valid_students = []

        for student in students:
            if name in student.name:
                valid_students.append(student)

        if len(valid_students) == 0:
            return []

        valid_students = sorted(valid_students, key=lambda x: x.name)

        return valid_students

    
    def get_student_by_id(self, id) -> object:
        try:
            id = int(id)
        except ValueError:
            raise ServiceException('Id must be an intger')
            
        return self.repository.get_item_by_id(id)