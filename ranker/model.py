from google.appengine.ext import ndb


class App(ndb.Model):
    name = ndb.StringProperty(indexed=False)
    ranker = ndb.KeyProperty(indexed=False)


class Ranker(ndb.Model):
    score_range = ndb.IntegerProperty(indexed=False, repeated=True)
    branching_factor = ndb.IntegerProperty(indexed=False)


class RankerScore(ndb.Model):
    value = ndb.IntegerProperty(indexed=False, repeated=True)


class RankerNode(ndb.Model):
    child_counts = ndb.IntegerProperty(indexed=False, repeated=True)


