from random import randint
from car import Car
from car import generate_cars
import pickle


class RepositoryError(Exception):
    pass

class CarRepositoryBinaryFile:
    def __init__(self, file_name: str):
        self.__file_name = file_name
        self.__data = []
        self.__load_file()

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
        self.__save_file()

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

        self.__save_file()
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

    def __save_file(self):
        """
        This method is private because:
            1. the class is responsible for writing its state to file
            2. we want file operations toe be unseen to the user
        """
        file = open(self.__file_name, "wb") # w - write, b - binary
        pickle.dump(self.__data, file)
        file.close()

    def __load_file(self):
        """
        
        """
        try:
            file = open(self.__file_name, "rb") # r - read, b - binary
            self.__data = pickle.load(file)
            file.close()
        except FileNotFoundError:
            print("We ate this exception. It was yummy!")
        except OSError:
            # the ui does not know we use files
            raise RepositoryError("Cannot start repository")


bin_repo = CarRepositoryBinaryFile("mmcars.data")

for car in bin_repo.get_all():
    print(car)

# for car in generate_cars(10):
#     bin_repo.add(car)