import unittest
from service import ReservationService
from service import ServiceException
from repository import ReservationRepository


class TestReservationService(unittest.TestCase):
    def setUp(self):
        repo = ReservationRepository('reservations.txt')
        self.service = ReservationService(repo)

    def test_nothing(self):
        self.assertEqual(1, 1)




if __name__ == '__main__':
    unittest.main()