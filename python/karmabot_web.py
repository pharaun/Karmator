import json

from klein import Klein
from twisted.web.iweb import IRenderable
from twisted.web.server import Site
from twisted.web.template import tags
from zope.interface import implementer

import karmabot_models as models


@implementer(IRenderable)
class TagElement(object):
    def __init__(self, tag):
        self.tag = tag

    def render(self, request):
        return self.tag


class KarmaWeb(object):
    app = Klein()

    def __init__(self, Session):
        self.Session = Session

    @app.route('/')
    def index(self, request):
        return TagElement(tags.h1('hi!'))

    sortkeys = {
        'up': lambda t: t[1][0],
        'down': lambda t: t[1][1],
        'both': lambda t: t[1][0] + t[1][1],
        'karma': lambda t: t[1][0] - t[1][1],
    }

    tables = {
        'receiving': models.karma_in,
        'giving': models.karma_out,
    }

    @app.route('/<string:table>/<string:sortkey>/votes.svg')
    def vote_chart(self, request, table, sortkey):
        request.setHeader('Content-Type', 'image/svg+xml')
        return models.all_counts_graph(
            self.Session, self.tables[table], self.sortkeys[sortkey])

    @app.route('/api/karma.json')
    def karma_dump(self, request):
        request.setHeader('Content-Type', 'application/json')
        d = models.all_karma(self.Session)
        d.addCallback(lambda r: json.dumps(dict(tuple(x) for x in r)))
        return d

    @app.route('/api/all-counts.json')
    def karma_count_dump(self, request):
        request.setHeader('Content-Type', 'application/json')
        d = models.all_counts(self.Session, models.karma_in)
        d.addCallback(json.dumps)
        return d


def build_site(Session):
    return Site(KarmaWeb(Session).app.resource())
