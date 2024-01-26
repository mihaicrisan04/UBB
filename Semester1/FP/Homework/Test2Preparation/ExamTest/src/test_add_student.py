import unittest
from repository import Repository
from service import Service
from domain import Student


class TestAddStudent:
    def setUp(self):
        self.repository = Repository('students.txt')
        self.service = Service(self.repository)

    def test_add_student(self):
        student = Student(200, 'Hatz John', 12, 6)
        self.service.add_sudent('200', 'Hatz John', '12', '6')
        self.assertEqual(self.service.get_student_by_id('200'), student)


if __name__ == '__main__':
    unittest.main()