import unittest
from sentence import Sentence
from service import SentenceService, ServiceException
from repository import SentenceRepository, RepositoryException
from ui import UI


class TestSentence(unittest.TestCase):
    def setUp(self):
        self.sentence = Sentence.from_string("A sentence")

    def test_sentence(self):
        self.assertEqual(str(self.sentence), "a sentence")

    def test_preview_string(self):
        self.assertEqual(self.sentence.preview_string, "a se__e__e")


class TestSentenceService(unittest.TestCase):
    def setUp(self):
        self.sentence_service = SentenceService(SentenceRepository("test_sentences.txt"))

    def test_add_sentence(self):
        self.sentence_service.add_sentence("One sentence")
        self.sentence_service.add_sentence("Another sentence")
        self.assertEqual(len(self.sentence_service.get_all_sentences()), 2)



if __name__ == "__main__":
    unittest.main()