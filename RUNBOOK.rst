starting karmator
=================

Command line arguments are, respectively: SQLAlchemy database URL, IRC server
to connect to, web port to listen on, and then arguments for the connection.
For example, for running karmator on localhost, connecting to the ZNC for karmator
also on localhost::

  python karmabot.py sqlite:///db.sqlite ssl:localhost:26697 tcp:6080 nickname=karmator password=karmator:karmator

The ``karmator:karmator`` for the IRC password is ZNC syntax specifying that
the username is ``karmator`` and the password is ``karmator``. Arguments are
set as attributes on the `IRCClient`_ instance, so anything described as an
instance attribute is a valid argument.


making karmator join a channel
==============================

Super easy::

  /invite karmator #the-channel


restarting the karma parser
===========================

1. Locate the pid of the karma parser. ``ps f`` is good for this. It'll be
   running as a child of the karmator python process.

2. Send it ``SIGTERM``. Karmator will automatically restart it by calling
   ``./karma-parser`` in its current working directory.


.. _IRCClient: http://twistedmatrix.com/documents/current/api/twisted.words.protocols.irc.IRCClient.html
