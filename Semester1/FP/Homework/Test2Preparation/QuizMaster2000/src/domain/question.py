

class Question:
    def __init__(self, id: int, text: str, choice_a: str, choice_b: str, choice_c: str, correct_choice: str, difficulty):
        self.id = id
        self.text = text
        self.choice_a = choice_a
        self.choice_b = choice_b
        self.choice_c = choice_c
        self.correct_choice = correct_choice
        self.difficulty = difficulty

    def __str__(self) -> str:
        return f'{self.id};{self.text};{self.choice_a};{self.choice_b};{self.choice_c};{self.correct_choice};{self.difficulty}'

    def __eq__(self, other) -> bool:
        if not isinstance(other, Question):
            return False
        return self.id == other.id