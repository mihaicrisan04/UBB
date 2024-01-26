import unittest
from src.repository.repository_buses import BusRepository
from src.repository.repository_exception import RepositoryException
from src.service.service_buses import BusService
from src.service.service_exception import ServiceException


class TestBusService(unittest.TestCase):
    def setUp(self):
        self.bus_repo = BusRepository('buses.txt')
        self.bus_service = BusService(self.bus_repo)

    def test_get_buses(self):
        self.assertEqual(self.bus_service.get_buses(), self.bus_service.get_buses())



if __name__ == '__main__':
    unittest.main()