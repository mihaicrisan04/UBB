import unittest
from src.domain.bus import Bus


class TestBus(unittest.TestCase):
    def setUp(self):
        self.bus = Bus(1, 1, 'mercedes', 10)

    def test_bus_id(self):
        self.assertEqual(self.bus.bus_id, 1)

    def test_route_code(self):
        self.assertEqual(self.bus.route_code, 1)


if __name__ == '__main__':
    unittest.main()