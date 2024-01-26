from unittest import TestCase, TextTestRunner, TestSuite, TestLoader
from tests.test_car import TestCar
# from tests.test_client import TestClient

def suite():
    suite = TestSuite()
    suite.addTest(TestLoader().loadTestsFromTestCase(TestCar))
    
    return suite

if __name__ == '__main__':
    runner = TextTestRunner()
    runner.run(suite())