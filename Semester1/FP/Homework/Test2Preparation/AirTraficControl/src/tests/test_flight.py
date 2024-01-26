import unittest
from src.domain.flight import Flight


class TestFlight(unittest.TestCase):
    def setUp(self):
        self.flight = Flight('RO650', 'Cluj', '05:45', 'Bucuresti', '06:40')

    def test_flight_id(self):
        self.assertEqual(self.flight.flight_id, 'RO650')
    
    def test_departure_city(self):
        self.assertEqual(self.flight.departure_city, 'Cluj')

    def test_departure_time(self):
        self.assertEqual(self.flight.departure_time, '05:45')

    def test_arrival_city(self):
        self.assertEqual(self.flight.arrival_city, 'Bucuresti')

    def test_arrival_time(self):
        self.assertEqual(self.flight.arrival_time, '06:40')


if __name__ == '__main__':
    unittest.main()