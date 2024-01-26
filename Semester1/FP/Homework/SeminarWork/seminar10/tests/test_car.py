from domain.car import Car
from unittest import TestCase

class TestCar(TestCase):

    def test_car_license_plate(self):
        c = Car("CJ 10 QWE", "Toyota", "Corola", "green")

        self.assertEqual(c.license_plate, "CJ 10 QWE")

    def test_car_make(self):
        c = Car("CJ 10 QWE", "Toyota", "Corola", "green")

        self.assertEqual(c.make, "Toyota")

