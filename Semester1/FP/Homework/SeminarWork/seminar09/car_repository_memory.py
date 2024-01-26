from random import randint
from car import Car
from car import generate_cars
import pickle


class RepositoryError(Exception):
    pass

class CarRepositoryMemory:
    def __init__(self):
        self.__data = []

    def add(self, new_car: Car):
        """
        Add the new car to the repo
        :param new_car:
        :return:
        Raise RepositoryError if car with this ilcense plate already in repo
        """
        for i in range(len(self.__data)):
            if self.__data[i].license_plate == new_car.license_plate:
                raise RepositoryError(f"Car with license plate {new_car.license_plate} already in repo")
        self.__data.append(new_car)

    def remove(self, license_plate: str) -> Car:
        """
        Remove the car with the given license plate
        :param license_plate:
        :return: the removed car
        Raise RepositoryError if car with license plate not in repo
        """
        ok = False
        for i in range(len(self.__data)):
            if self.__data[i].license_plate == license_plate:
                car_to_remove = self.__data[i]
                self.__data.remove(self.__data[i])
                ok = True
                break

        if ok == False:
            raise RepositoryError(f"Car with license plate {license_plate} not in repo")
        return car_to_remove

    def get_all(self) -> list:
        """
        Return all cars in repo
        :return:
        """
        return self.__data

    def __len__(self):
        """
        Retrun tehe numberof cars in the repository
        """
        return len(self.__data)


def test_car_repo_memory(): 
    repo = CarRepositoryMemory()
    assert len(repo) == 0 # repo should be empty

    # try to add cars
    car_list = generate_cars(10)
    repo_len = 0
    for car in car_list:
        repo.add(car)
        repo_len += 1
        assert len(repo) == repo_len

    # try to add a car with the same license plate
    try:
        repo.add(car_list[0]) # this is aready in the repo
        assert False # code should raise exception
    except RepositoryError:
        # we expect a RepoError here
        assert True

    # test remove from repository
    for car in car_list:
        # remove returns the car that it deletes
        assert repo.remove(car.license_plate) == car
        repo_len -= 1
        assert len(repo) == repo_len

    assert len(repo) == 0 # repo should be empty

    # try to remove a car that is not in the repo
    try:
        repo.remove("B 01 ABC") # this is not in the repo
        assert False # code should raise exception
    except RepositoryError:
        # we expect a RepoError here
        assert True

# test_car_repo_memory()

# 1 write to binary file
# car_list = generate_cars(10)
# file = open("cars.data", "wb") # w - write, b - binary
# pickle.dump(car_list, file)
# file.close()

# 2 read from binary file
# file = open("cars.data", "rb") # r - read, b - binary
# cars = pickle.load(file)
# file.close()

# for car in cars:
#     print(car)

"""
How do we integrate read/write binary files to the repository
"""