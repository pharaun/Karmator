import json
import sys

from twisted.internet.defer import Deferred, DeferredQueue, gatherResults
from twisted.internet.endpoints import clientFromString, ProcessEndpoint
from twisted.internet.protocol import Factory
from twisted.internet.task import react, deferLater
from twisted.protocols.basic import LineOnlyReceiver
from twisted.python import log
from twisted.words.protocols.irc import IRCClient

import karmabot_models as models


class KarmaProcessorProtocol(LineOnlyReceiver):
    delimiter = '\n'

    def __init__(self):
        self.finished = Deferred()

    def connectionMade(self):
        self._pump_queue()
        self.transport.disconnecting = False  # compatibility hack

    def _pump_queue(self):
        self.queue_deferred = self.factory.in_queue.get()
        self.queue_deferred.addCallback(self._got_queue_item)
        self.queue_deferred.addErrback(log.err)

    def _got_queue_item(self, item):
        self.sendLine(json.dumps(item))
        self._pump_queue()
        print 'out', item

    def lineReceived(self, line):
        line_data = json.loads(line)
        print 'in', line_data

    def connectionLost(self, reason):
        self.queue_deferred.cancel()
        self.finished.errback(reason)


class KarmaProcessorFactory(Factory):
    protocol = KarmaProcessorProtocol

    def __init__(self, Session, in_queue):
        self.Session = Session
        self.in_queue = in_queue


class Karmabot(IRCClient):
    def __init__(self):
        self.finished = Deferred()

    def privmsg(self, user, channel, message):
        item = {'user': user, 'channel': channel, 'message': message}
        self.factory.out_queue.put(item)

    def irc_INVITE(self, prefix, params):
        self.join(params[1])

    def connectionLost(self, reason):
        self.finished.errback(reason)


class KarmabotFactory(Factory):
    protocol = Karmabot

    def __init__(self, Session, kwargs, out_queue):
        self.Session = Session
        self.kwargs = kwargs
        self.out_queue = out_queue

    def buildProtocol(self, addr):
        proto = Factory.buildProtocol(self, addr)
        for k, v in self.kwargs:
            setattr(proto, k, v)
        return proto


def main(reactor, database_uri, endpoint_description, *args):
    engine, Session = models.connect(database_uri)
    irc_endpoint = clientFromString(reactor, endpoint_description)
    kwargs = [arg.split('=', 1) for arg in args]
    karma_queue = DeferredQueue()
    irc_fac = KarmabotFactory(Session, kwargs, karma_queue)

    karma_parser_endpoint = ProcessEndpoint(reactor, './karma-processor', ['./karma-processor'])
    karma_parser_fac = KarmaProcessorFactory(Session, karma_queue)

    def connect_func(endpoint, fac):
        def connect(starting=False):
            if not reactor.running and not starting:
                return
            d = endpoint.connect(fac)
            d.addCallback(lambda proto: proto.finished)
            d.addErrback(log.err)
            d.addCallback(lambda ign: deferLater(reactor, 10, connect))
            return d
        return connect

    connect_irc = connect_func(irc_endpoint, irc_fac)
    connect_karma_parser = connect_func(karma_parser_endpoint, karma_parser_fac)

    return gatherResults(
        [connect_irc(starting=True), connect_karma_parser(starting=True)])

log.startLogging(sys.stdout)
react(main, sys.argv[1:])
