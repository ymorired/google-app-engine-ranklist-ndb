import unittest
import os
import json
from pprint import pprint

from ranker import ranker


def create_filled_ranking():
    ranking = ranker.Ranker.get_or_create('default', [0, 1000], 100)
    ranking.set_score('user1', [90])
    ranking.set_score('user1', [100])
    ranking.set_score('user2', [130])
    ranking.set_score('user3', [130])
    ranking.set_scores({
        'user4': [140],
        'user5': [150]
    })
    return ranking


class BasicTest(unittest.TestCase):

    def setUp(self):
        pass

    def test_create(self):
        ranker.Ranker.create('default', [0, 10000], 100)

    def test_set_score(self):
        create_filled_ranking()

    def test_get_score(self):
        ranking = create_filled_ranking()

        self.assertEqual(ranking.get_score('user1'), [100])
        self.assertEqual(ranking.get_score('user2'), [130])

        self.assertEqual(
            ranking.get_scores(['user3', 'user4', 'user_none']),
            [[130], [140], None]
        )

    def test_find_rank(self):
        """
            0-based ranking
        """
        ranking = create_filled_ranking()

        self.assertEqual(ranking.find_rank([150]), 0)
        self.assertEqual(ranking.find_rank([140]), 1)
        self.assertEqual(ranking.find_rank([100]), 4)

        self.assertEqual(
            # rank 5 does not exist
            ranking.find_ranks([[130], [100], [90]]),
            [2, 4, 5]
        )

    def test_find_score(self):
        ranking = create_filled_ranking()

        self.assertEqual(ranking.find_score(0), ([150], 0))
        self.assertEqual(ranking.find_score(2), ([130], 2))
        self.assertEqual(ranking.find_score(3), ([130], 2))
        self.assertEqual(ranking.find_score(5), None)

    def test_total_num(self):
        ranking = create_filled_ranking()

        self.assertEqual(ranking.total_ranked_player_num(), 5)

if __name__ == '__main__':
    unittest.main()
