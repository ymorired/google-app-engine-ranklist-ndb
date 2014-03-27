google-app-engine-ranklist-ndb
==============================

google-app-engine-ranklist-ndb is a forked project from https://code.google.com/p/google-app-engine-ranklist/

Following changes are made to the original code

* use ndb instead of raw datastore
* get_score function with player name
* basic test code

This project include example server code from original repository. run ```dev_appserver.py example``` to start example code.


About Ranklist
==============

Ranklist is a python library for Google Appengine that implements a
data structure for storing integer scores and quickly retrieving their
relative ranks.

See the docstrings in ranker/ranker.py for more details and usage
examples.


Example
=======

An example application can be found in the "examples" directory. You
can run it with the following command:

$ dev_appserver.py example

