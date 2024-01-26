from repository import RepositoryException
import random

class ServiceException(Exception):
    def __init__(self, message):
        super().__init__()


class Service:
    def __init__(self, repository):
        self.repository = repository

    def get_all(self) -> list:
        return self.repository.get_all()


    def get_players_by_strength(self) -> list:
        players = self.get_all()
        return sorted(players, key= lambda x: x.strength, reverse=True)


    def admisable_players_count(self) -> int:
        players = self.get_all()

        players_count = len(players)

        admisable_players = 1
        while admisable_players <= players_count:
            admisable_players *= 2

        admisable_players //= 2

        return admisable_players

    def get_pairings(self, players) -> list:
        pairings = []

        while len(players) > 0:
            first_index = random.randint(0, len(players) - 1)

            second_index = random.randint(0, len(players) - 1)
            while second_index == first_index:
                second_index = random.randint(0, len(players) - 1)

            if second_index > first_index:
                first_index, second_index = second_index, first_index

            first_player = players.pop(first_index)
            second_player = players.pop(second_index)

            pairings.append((first_player, second_player))

        return pairings


    def get_round_pairings(self) -> list:
        """
        Function that randomly pairs two players to form a round and 
        returns a list with all the pairings formed by the current state of the players

        params:
            None:

        """
        players = self.get_all()
        pairings = self.get_pairings(players)

        return pairings


    
    def get_qualifications_pairings(self, players_count, admisable_players_count) -> list:
        """
        Function that pairs the least stronges players to form a round

        params:
            players_count: the number of players in the current state(qualification round)
            admisable_players_count = the maximum number of players allowd to start a to
        
        """
        players = self.get_all()
        players = sorted(players, key= lambda x: x.strength)

        number_of_rounds = players_count - admisable_players_count

        players = players[:number_of_rounds * 2]

        pairings = self.get_pairings(players)

        return pairings

        
    def delete_player(self, id: int):
        try:
            self.repository.delete_player(id)
        except RepositoryException as re:
            raise ServiceException(re)
