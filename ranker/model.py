from google.appengine.ext import ndb


class App(ndb.Model):
    name = ndb.StringProperty(indexed=False)
    ranker = ndb.KeyProperty(indexed=False)


class Ranker(ndb.Model):
    """

    """
    score_range = ndb.IntegerProperty(indexed=False, repeated=True)
    branching_factor = ndb.IntegerProperty(indexed=False)


class RankerScore(ndb.Model):
    """
        key = player_id
    """
    value = ndb.IntegerProperty(indexed=False, repeated=True)


class RankerNode(ndb.Model):
    """
        key = node_id
    """
    child_counts = ndb.IntegerProperty(indexed=False, repeated=True)


