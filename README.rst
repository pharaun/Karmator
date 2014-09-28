Karmator
--------

This is a Haskell reimplementation of `karmator` the early protype karma bot.

But basically its broken up into a couple parts:

1. The bot core which is implemented in Haskell, it has no karma/bot specific part. Its just responsible for all of the network+bot management stuff.

2. A upper layer which implements more advanced features such as plugins, and so forth.

3. The karma portion which is directly inherited from the previous edition of `karmator`

This karma portion has several improved abilities such as:

1. Support for viewing karma stats/graph in your browser (Not currently implemented)

2. Support for up/down/side votes

3. More fancy parser which supports multiple karma expression in one sentence and braced karma expression and so on.
