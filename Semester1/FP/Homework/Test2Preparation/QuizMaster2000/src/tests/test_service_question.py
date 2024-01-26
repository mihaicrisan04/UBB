import unittest
from src.services.service_questions import QuestionService
from src.repository.repository_questions import QuestionRepository
from src.services.service_exception import ServiceException
from src.repository.repository_exception import RepositoryException


class TestService(unittest.TestCase):
    def setUp(self):
        pass

    def test_load(self):
        self.assertEqual(1, 1)


if __name__ == '__main__':
    unittest.main()