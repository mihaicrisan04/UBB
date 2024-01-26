

class Sentence:
    def __init__(self, sentence: str):
        self.sentence_string = sentence.strip().lower()
        self.guessed_letters = set()


    def __eq__(self, other) -> bool:
        if not isinstance(other, Sentence):
            return False
        return self.sentence_string == other.sentence_string


    def __str__(self) -> str:
        return self.sentence_string

    def guess_letter(self, letter: str) -> bool:
        self.guessed_letters.add(letter)




    @staticmethod
    def from_string(sentence: str) -> 'Sentence':
        return Sentence(sentence)
