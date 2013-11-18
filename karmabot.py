import json
import shlex
import sys

from twisted.internet import defer, endpoints
from twisted.internet.protocol import Factory
from twisted.internet.task import react, deferLater
from twisted.protocols.basic import LineOnlyReceiver
from twisted.python import log
from twisted.words.protocols.irc import IRCClient

import karmabot_models as models
import karmabot_web as web


class KarmaProcessorProtocol(LineOnlyReceiver):
    delimiter = '\n'

    def __init__(self):
        self.finished = defer.Deferred()

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

    def lineReceived(self, line):
        line_data = json.loads(line)
        if not line_data['karma']:
            return
        models.add_karma(self.factory.Session, line_data).addErrback(log.err)

    def connectionLost(self, reason):
        self.queue_deferred.cancel()
        self.finished.errback(reason)


class KarmaProcessorFactory(Factory):
    protocol = KarmaProcessorProtocol

    def __init__(self, Session, in_queue):
        self.Session = Session
        self.in_queue = in_queue


def canonize(things):
    return [thing.strip().decode('utf-8') for thing in things]


class Karmabot(IRCClient):
    def __init__(self):
        self.finished = defer.Deferred()

    def privmsg(self, user, channel, message):
        nick, _, host = user.partition('!')
        if channel.startswith('#'):
            item = {'user': user, 'nick': nick, 'channel': channel, 'message': message}
            self.factory.out_queue.put(item)
        else:
            channel = nick

        if not message.startswith('!'):
            return

        splut = shlex.split(message[1:])
        command, params = splut[0], splut[1:]
        meth = getattr(self, 'command_%s' % (command.lower(),), None)
        if meth is not None:
            d = defer.maybeDeferred(meth, nick, channel, *params)
            d.addErrback(log.err)

    def message_channels(self, message, channels):
        if isinstance(message, unicode):
            message = message.encode('utf-8')
        for channel in channels:
            self.msg(channel, message)

    def format_karma(self, things):
        return '; '.join(('%s (%s)' % tuple(thing)) for thing in things)

    def format_general_stats(self, stats, nick):
        highest, lowest, n_higher, total = stats
        return '%s, highest karma: %s. lowest karma: %s. your rank is %d of %d.' % (
            nick, self.format_karma(highest), self.format_karma(lowest), n_higher + 1, total)

    def format_count(self, count):
        name, (p, n, s) = count
        return '%s, %s (%s++/%s--/%s+-)' % (name, p - n, p, n, s)

    def format_counts(self, counts, nick):
        return '%s: %s' % (nick, '; '.join(self.format_count(count) for count in counts))

    def command_karma(self, nick, channel, *users):
        nick = nick.decode('utf-8')
        users = canonize(users)
        if not users:
            d = models.general_stats(self.factory.Session, 3, nick)
            d.addCallback(self.format_general_stats, nick)
        else:
            d = models.counts(self.factory.Session, users)
            d.addCallback(self.format_counts, nick)
        d.addCallback(self.message_channels, [channel])

    def format_giver_stats(self, stats, nick):
        highest, lowest, n_higher, total = stats
        return '%s, most positive: %s. most negative: %s. your rank is %d of %d in positivity.' % (
            nick, self.format_karma(highest), self.format_karma(lowest), n_higher + 1, total)

    def command_karmagivers(self, nick, channel, *users):
        nick = nick.decode('utf-8')
        users = canonize(users)
        if not users:
            d = models.giver_stats(self.factory.Session, 3, nick)
            d.addCallback(self.format_giver_stats, nick)
        else:
            d = models.counts(self.factory.Session, users, table=models.karma_out)
            d.addCallback(self.format_counts, nick)
        d.addCallback(self.message_channels, [channel])

    def format_sidevote_stats(self, stats, nick):
        received, given, n_higher_received, total_received, n_higher_given, total_given = stats
        return ('%s, most sidevotes received: %s. most sidevotes given: %s. '
                'your rank is %d of %d in giving and %d of %d in receiving.') % (
                    nick, self.format_karma(received), self.format_karma(given),
                    n_higher_received + 1, total_received,
                    n_higher_given + 1, total_given)

    def command_sidevotes(self, nick, channel):
        nick = nick.decode('utf-8')
        d = models.sidevote_stats(self.factory.Session, 3, nick)
        d.addCallback(self.format_sidevote_stats, nick)
        d.addCallback(self.message_channels, [channel])

    def command_karmatorjoin(self, nick, channel, channel_to_join):
        self.join(channel_to_join)

    def command_karmatorleave(self, nick, channel):
        self.part(channel)

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


def main(reactor, database_uri, irc_endpoint, web_endpoint, *args):
    engine, Session = models.connect(database_uri)
    irc_endpoint = endpoints.clientFromString(reactor, irc_endpoint)
    kwargs = [arg.split('=', 1) for arg in args]
    karma_queue = defer.DeferredQueue()
    irc_fac = KarmabotFactory(Session, kwargs, karma_queue)

    karma_parser_endpoint = endpoints.ProcessEndpoint(reactor, './karma-parser', args=['./karma-parser', 'KarmaParser/parser.cfg'])
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

    web_endpoint = endpoints.serverFromString(reactor, web_endpoint)
    web_site = web.build_site(Session)

    return defer.gatherResults(
        [connect_irc(starting=True), connect_karma_parser(starting=True),
         web_endpoint.listen(web_site)])

log.startLogging(sys.stdout)
react(main, sys.argv[1:])
