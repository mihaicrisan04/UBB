from constants import *


class UI:
    def __init__(self, service):    
        self.service = service

    def print_menu(self):
        print(ADD_SENTENCE_OPTION + ". Add a sentence")
        print(START_GAME_OPTION + ". Start game")
        print(EXIT_OPTION + ". Exit")

    
    def run_game(self):
        sentence = self.service.get_random_sentence()
        hang_count = 0
        hangman = "hangman"

        while True:
            guess = input("Enter guess: ").strip().lower()

            if len(guess) != 1:
                print("Invalid guess!")
                continue

            if not guess.isalpha():
                print("Invalid guess!")
                continue

            # check if the letter was already guessed
            if guess in sentence.guessed_letters:
                print("Letter already guessed!")
                continue

            # check if the letter is in the sentence
            if not sentence.guess_letter(guess):
                print("Letter not in sentence!")
                hang_count += 1

            # check if the game is over
            if hang_count == 7:
                print("You lost!")
                print("The sentence was: " + str(sentence))
                break

            # print the sentence with the guessed letters
            output = ""
            for letter in sentence.sentence_string:
                if letter in sentence.guessed_letters:
                    output += letter
                else:
                    output += "_"   

            output += " - " + hangman[:hang_count]
            print(output)




    def run(self):
        while True:
            self.print_menu()

            option = input("Enter option: ")

            if option == ADD_SENTENCE_OPTION:
                sentence = input("Enter sentence: ").strip()

                words = sentence.split()
                if len(words) < 1:
                    print("Invalid sentence!")
                    continue

                for word in words:
                    if len(word) < 3:
                        print("Invalid sentence!")
                        continue

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

    