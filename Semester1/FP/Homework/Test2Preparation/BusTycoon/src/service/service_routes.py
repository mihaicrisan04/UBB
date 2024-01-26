

class RouteService:
    def __init__(self, route_repository):
        self.__route_repository = route_repository

    def get_routes(self) -> list:
        return self.__route_repository.get_all()