from sentence import Sentence


class RepositoryException(Exception):
    def __init__(self, message):
        super().__init__(message)


class SentenceRepository:
    def __init__(self, file_name: str):
        self.sentences = []
        self.file_name = file_name    
        self.read_data()


    def read_data(self):
        try:
            with open(self.file_name, "r") as file:
                for row in file:
                    row = row.strip()
                    if row == "":
                        continue
                    sentence = Sentence.from_string(row)
                    self.add_sentence(sentence)
        except FileNotFoundError:
            raise RepositoryException("File not found!")


    def save_data(self):
        with open(self.file_name, "w") as file:
            for sentence in self.sentences:
                file.write(str(sentence) + "\n")

    
    def add_sentence(self, sentence: Sentence):
        for s in self.sentences:
            if s == sentence:
                raise RepositoryException("Sentence already exists!")
        self.sentences.append(sentence)
        self.save_data()

