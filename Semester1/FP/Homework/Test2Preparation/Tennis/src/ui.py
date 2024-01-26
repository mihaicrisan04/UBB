from service import ServiceException


class UI:
    def __init__(self, service):
        self.service = service

        #Magic Numbers
        self.QUARTER_FINALS = 8
        self.SEMI_FINALS = 4
        self.FINALS = 2

    def pick_player(self) -> str:
        while True:
            pick = input('Chose the winner: ').strip()
            if pick == '1':
                return 1
            elif pick == '2':
                return 2
            else:
                print('Invalid pick')
            
    
    def play_round(self, player1, player2):
        print('1. ' + str(player1))
        print('2. ' + str(player2))

        pick = self.pick_player()

        if pick == 1:
            return player2
        return player1


    def run(self):  
        players = self.service.get_players_by_strength()

        # print the players in reverse order by strength
        for player in players:
            print(player)

        players_count = len(players)
        admisable_players_count = self.service.admisable_players_count()

        if players_count != admisable_players_count:
            print('Playing qualifing round')

            pairings = self.service.get_qualifications_pairings(players_count, admisable_players_count)

            for pair in pairings:
                player1 = pair[0]
                player2 = pair[1]

                print(f'qualifications: ')
                loser = self.play_round(player1, player2)
                
                try:
                    self.service.delete_player(loser.id)
                except ServiceException as se:
                    print(se)


        print('Starting the tournament')
        
        tournament_winner = None
        tournament_end = False

        while tournament_end == False:
            players = self.service.get_all()
            pairings = self.service.get_round_pairings()

            round_info = ''
            if len(players) == self.QUARTER_FINALS:
                round_info = 'Last 8:'
            elif len(players) == self.SEMI_FINALS:
                round_info = 'Last 4:'
            elif len(players) == self.FINALS:
                round_info = 'Last 2:'
                tournament_end = True

            print(round_info)
            for pair in pairings:
                player1 = pair[0]
                player2 = pair[1]

                loser = self.play_round(player1, player2)
                winner = player1 if loser != player1 else player2
                winner.strength += 1

                if len(players) == self.FINALS:
                    tournament_winner = winner
 
                try:
                    self.service.delete_player(loser.id)
                except ServiceException as se:
                    print(se)

        print(f'Winnder is: {tournament_winner}')




        

