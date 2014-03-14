#!/usr/bin/python
#
# Copyright 2008 Google Inc.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
#      http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
import os

from google.appengine.ext import webapp
from google.appengine.ext.webapp import template
from google.appengine.ext.webapp.util import run_wsgi_app

import ranker.ranker as ranker


APP_KEY = 'default'
MIN_SCORE = 0
MAX_SCORE = 9999


def get_ranker():
    return ranker.Ranker.get_or_create(APP_KEY, [MIN_SCORE, MAX_SCORE + 1], 100)


def show_error_page(self, error):
    template_values = {"error": error}
    path = os.path.join(os.path.dirname(__file__), 'error.html')
    self.response.out.write(template.render(path, template_values))


class MainPage(webapp.RequestHandler):
    def get(self):
        path = os.path.join(os.path.dirname(__file__), 'index.html')
        self.response.out.write(template.render(path, {}))


class SetScoreHandler(webapp.RequestHandler):
    def post(self):
        score = self.request.get("score")
        name = self.request.get("name")
        try:
            assert len(name) > 0
            assert name[0] not in "0123456789"
            score = int(score)
            assert MIN_SCORE <= score <= MAX_SCORE
        except Exception as e:
            show_error_page(self, "Your name must not be empty, and must not start with "
                                  "a digit.  In addition, your score must be an integer "
                                  "between 0 and 9999, inclusive. %s" % e.message)
            return
        r = get_ranker()
        r.set_score(name, [score])
        self.redirect("/")


class QueryRankPage(webapp.RequestHandler):
    def get(self):
        r = get_ranker()
        rank = int(self.request.get("rank"))
        if rank >= r.total_ranked_player_num():
            show_error_page(self, "There aren't %d ranked people!" % (rank + 1))
            return

        (score, rank_at_tie) = r.find_score(rank)
        template_values = {"score": score[0], "rank": rank}
        if rank_at_tie < rank:
            template_values["tied"] = True
            template_values["rank_at_tie"] = rank_at_tie

        path = os.path.join(os.path.dirname(__file__), 'rank.html')
        self.response.out.write(template.render(path, template_values))


class QueryScorePage(webapp.RequestHandler):
    def get(self):
        r = get_ranker()
        try:
            score = int(self.request.get("score"))
            assert MIN_SCORE <= score <= MAX_SCORE
        except Exception as e:
            show_error_page(self, "Scores must be integers between 0 and 9999 inclusive. %s" % e.message)
            return
        rank = r.find_rank([score])
        template_values = {"score": score, "rank": rank}

        path = os.path.join(os.path.dirname(__file__), 'score.html')
        self.response.out.write(template.render(path, template_values))


application = webapp.WSGIApplication([
    ('/', MainPage),
    ('/setscore', SetScoreHandler),
    ('/getrank', QueryRankPage),
    ('/getscore', QueryScorePage)
], debug=True)


def main():
    run_wsgi_app(application)

if __name__ == "__main__":
    main()
