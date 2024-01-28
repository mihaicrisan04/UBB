

class Sentence:
    def __init__(self, sentence: str):
        self.sentence_string = sentence.strip().lower()
        self.guessed_letters = set()
        self.preview_string = self.__get_preview_string()

    def __eq__(self, other) -> bool:
        if not isinstance(other, Sentence):
            return False
        return self.sentence_string == other.sentence_string

    def __str__(self) -> str:
        return self.sentence_string


    def __get_preview_string(self) -> str:
        self.guessed_letters.add(self.sentence_string[0])
        self.guessed_letters.add(self.sentence_string[-1])
        for i in range(1, len(self.sentence_string) - 1, 1):
            if self.sentence_string[i].isalpha() and self.sentence_string[i - 1] == " " or self.sentence_string[i + 1] == " ":
                self.guessed_letters.add(self.sentence_string[i])

        output = ""
        for letter in self.sentence_string:
            if letter in self.guessed_letters or letter == " ":
                output += letter
            else:
                output += "_"

        return output


    def guess_letter(self, letter: chr) -> bool:
        letter = letter.lower()
        if letter in self.guessed_letters:
            return False
        if letter not in self.sentence_string:
            return False

        self.guessed_letters.add(letter)
        self.preview_string = self.__get_preview_string()
        return True


    @staticmethod
    def from_string(sentence: str) -> 'Sentence':
        return Sentence(sentence)
