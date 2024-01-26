

class Flight:
    def __init__(self, flight_id, departure_city, departure_time, arrival_city, arrival_time):
        self.__flight_id = flight_id
        self.__departure_city = departure_city
        self.__departure_time = departure_time
        self.__arrival_city = arrival_city
        self.__arrival_time = arrival_time

    def __str__(self):
        return self.__flight_id + ', ' + self.__departure_city + ', ' + self.__departure_time + ', ' + self.arrival_city + ', ' + self.arrival_time

    @property
    def flight_id(self):
        return self.__flight_id

    @property
    def departure_city(self):
        return self.__departure_city

    @property
    def departure_time(self):
        return self.__departure_time

    @property
    def arrival_city(self):
        return self.__arrival_city

    @property
    def arrival_time(self):
        return self.__arrival_time
