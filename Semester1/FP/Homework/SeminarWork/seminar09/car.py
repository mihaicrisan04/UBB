from random import randint

class Car:
    def __init__(self, license_plate, make, model, color):
        self.__license_plate = license_plate
        self.__make = make
        self.__model = model
        self.__color =  color

    def __str__(self):
        return f"Car: {self.__license_plate}, {self.__make}, {self.__model}, {self.__color}"

    def __eq__(self, other):
        return self.__license_plate == other.__license_plate

    def set_color(self, new_color):
        self.__color = new_color

    @property
    def license_plate(self):
        return self.__license_plate

    @property
    def make(self):
        return self.__make

    @property
    def model(self):
        return self.__model

    @property
    def color(self):
        return self.__color

    @color.setter
    def color(self, new_color):
        self.set_color(new_color)


def generate_cars(n: int) -> list:
    car_options = {
        "Toyota": ["Camry", "Corolla", "Prius", "Yaris"],
        "Honda": ["Civic", "Accord", "CRV", "Pilot"],
        "Ford": ["F-150", "Fusion", "Escape", "Explorer"],
        "Chevrolet": ["Silverado", "Equinox", "Malibu", "Cruze"],
        "Nissan": ["Altima", "Sentra", "Rogue", "Versa"],
        "Hyundai": ["Elantra", "Sonata", "Santa Fe", "Tucson"],
        "Kia": ["Forte", "Optima", "Soul", "Sorento"],
        "Jeep": ["Grand Cherokee", "Wrangler", "Cherokee", "Compass"],
        "Subaru": ["Outback", "Forester", "Crosstrek", "Impreza"],
        "GMC": ["Sierra", "Acadia", "Terrain", "Yukon"]
    }
    colors = ["Red", "Blue", "Green", "Yellow", "Black", "White", "Silver", "Gray"]
    conties = ["AB", "CJ", "B", "TL"]

    cars = []
    for i in range(n):
        make = list(car_options.keys())[randint(0, len(car_options) - 1)]
        model = car_options[make][randint(0, len(car_options[make]) - 1)]
        color = colors[randint(0, len(colors) - 1)]
        license_plate = conties[randint(0, len(conties) - 1)] + ' ' +  str(randint(10, 99)) + ' ' + chr(randint(65, 90)) + chr(randint(65, 90)) + chr(randint(65, 90))
        cars.append(Car(license_plate, make, model, color))

    return cars


if __name__ == '__main__':
    cars = generate_cars(10)
    for car in cars:
        print(car)
