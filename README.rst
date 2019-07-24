Karmator
--------

This is a Haskell reimplementation of `karmator` the early protype karma bot.

Basically its broken up in a couple parts:

1. karmator-core - implements all of the core bot + routing and provides some utility methods for other layers

2. karmator-(irc|slack} - implements the 'server' loop portion for connecting to irc or slack and the utilities needed

3. karmator-bin - the actual currently implemented bot bits n' pieces

4. karmator-karma - *TBD* may end up splitting out the karma + database logic out to its own chunk of work

5. karmator-batches - This is where all of the offline batches are stored such as the vote streak detectors for detecting people voting for one thing multiple times in a row.
