Karmator
--------

This is now the 4rd generation reimplementation of `karmator` which was:

1. Originally plugins of 2 separate irc bots.

2. Python irc bot + Haskell karma parser.

3. Pure Haskell irc bot.

4. Hybrid Haskell irc/slack bot (mostly slack2irc bridge)

5. Now this rust pure slack bot implementation.

Its broken up in two parts at the moment:

1. karmator-rust - This is the rust bot itself.

2. karmator-batches - This is where all of the offline batches are stored such as the vote streak detectors for detecting people voting for one thing multiple times in a row.
