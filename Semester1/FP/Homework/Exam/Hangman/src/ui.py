from constants import *
from service import ServiceException


class UI:
    def __init__(self, service):    
        self.service = service

    def print_menu(self):
        print()
        print(ADD_SENTENCE_OPTION + ". Add a sentence")
        print(START_GAME_OPTION + ". Start game")
        print(EXIT_OPTION + ". Exit")

    def validate_guess(self, guess: str) -> bool:
        if len(guess) != 1:
            return False
        if not guess.isalpha():
            return False
        return True

    
    def run_game(self):
        sentence = self.service.get_random_sentence()
        hang_count = 0
        hangman = "hangman"

        while True:
            output = sentence.preview_string + " - " + hangman[:hang_count]
            print(output)

            guess = input("Enter guess: ").strip().lower()

            if not self.validate_guess(guess):
                print("Invalid guess!")
                continue

            if not sentence.guess_letter(guess):
                hang_count += 1


            # check if the game is over
            if hang_count == 7:
                print(sentence.sentence_string + " - " + "You lost!") 
                break

            if sentence.preview_string == sentence.sentence_string:
                print(sentence.sentence_string + " - " + "You won!")
                break


    def run(self):
        while True:
            self.print_menu()

            option = input("Enter option: ")

            if option == ADD_SENTENCE_OPTION:
                sentence = input("Enter sentence: ").strip()

                try:
                    self.service.add_sentence(sentence)
                    print("Sentence added!")
                except ServiceException as se:
                    print(str(se))


            elif option == START_GAME_OPTION:
                self.run_game()

            elif option == EXIT_OPTION:
                break

            else:
                print("Invalid option!")

    