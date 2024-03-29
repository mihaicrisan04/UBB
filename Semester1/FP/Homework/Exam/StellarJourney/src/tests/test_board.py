import unittest
from src.domain.board import Board


class TestBoard(unittest.TestCase):
    def setUp(self):
        self.board = Board(8)
        self.board.update_board([(1, 1), (2, 2)], (3, 3), [(4, 4), (5, 5)])

    def test_update_board(self):
        self.assertEqual(self.board.board, [[0, 0, 0, 0, 0, 0, 0, 0], [0, 1, 0, 0, 0, 0, 0, 0], [0, 0, 1, 0, 0, 0, 0, 0], [0, 0, 0, 2, 0, 0, 0, 0], [0, 0, 0, 0, 3, 0, 0, 0], [0, 0, 0, 0, 0, 3, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0], [0, 0, 0, 0, 0, 0, 0, 0]])

    def test_cheat(self):
        self.assertEqual(self.board.cheat(), '+---+---+---+---+---+---+---+---+\n|   | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |\n+---+---+---+---+---+---+---+---+\n| A |   | * |   |   |   |   |   |   |\n+---+---+---+---+---+---+---+---+\n| B | * |   | * |   |   |   |   |   |\n+---+---+---+---+---+---+---+---+\n| C |   | * |   | * |   |   |   |   |\n+---+---+---+---+---+---+---+---+\n| D |   |   | * | E |   |   |   |   |\n+---+---+---+---+---+---+---+---+\n| E |   |   |   |   | B |   |   |   |\n+---+---+---+---+---+---+---+---+\n| F |   |   |   |   |   | B |   |   |\n+---+---+---+---+---+---+---+---+\n| G |   |   |   |   |   |   |   |   |\n+---+---+---+---+---+---+---+---+\n| H |   |   |   |   |   |   |   |   |\n+---+---+---+---+---+---+---+---+')



if __name__ == '__main__':
    unittest.main()
    