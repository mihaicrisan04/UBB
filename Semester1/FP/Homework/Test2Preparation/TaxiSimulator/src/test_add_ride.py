import unittest
from repository import Repository
from service import Service


class TestAddRide(unittest.TestCase):
    def setUp(self):
        self.repo = Repository()
        self.service = Service(self.repo)

        self.service.generate_taxis(1)


    def test_add_ride(self):

        x_start, y_start = 0, 0
        x_end, y_end = 10, 10

        self.service.calculate_ride('0,0', '10,10')

        taxi = self.service.get_all()[0]
        self.assertEqual(taxi.x, x_end)
        self.assertEqual(taxi.y, y_end)
        self.assertEqual(taxi.fare, 20)





if __name__ == '__main__':
    unittest.main()