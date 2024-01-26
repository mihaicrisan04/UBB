

class Student:
    def __init__(self, id: int, name: str, attendence: int, grade: int):
        self.id = id
        self.name = name
        self.attendence = attendence
        self.grade = grade


    def __str__(self) -> str:
        return f'{self.id}, {self.name}, {self.attendence}, {self.grade}'

    
    def __eq__(self, other) -> bool:
        if not isinstance(other, Student):
            return False
        return self.id == other.id