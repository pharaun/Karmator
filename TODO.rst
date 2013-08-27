TODO
----

1. Create a build system (make for setting up the project/etc) and (Cabal for the parser)

2. Importable corpus (Import from logfile, andor csv file for upgrading from other karma bots)

3. Upgrading script for upgrading from v1 to v2 of this system (need to implement Unicode Normalization, Case Folding, etc)


Parsing
=======

1. Primtive nickname defuzzer (Nick_1, nick|away, etc) (DONE)
    a. More advanced stateful tracking of nickname changes via hostmask/etc
    b. Registeration (for nickname defuzzer)
    c. Nickname defuzzer in the karma expression itself.

2. Simple Karma parsing (Multiple karma, simple postinc/dec/sides) (DONE)

3. Primitive Karma defuzzer (food ++, food++) (SEMI)
    a. Gives more trouble than its worth in the simple case, broken for c++, foobar --lol-gnu-options
    b. Need a more intelligent system

4. Nick name in the karma (Nick: win++) -> (win++)

5. Explicit support for braced karma expression such as "foo bar"++ -> foo bar++ for ex
    a. How to deal with natural braces, and nested braces and karma within the braces
    b. Ignore the karam within the braces
    c. Ignore nested braces

6. Intelligent Karma reduction
    a. foo+++++++ -> foo++
    b. Careful of #3.a - c++++ -> "c++"++ and c++--++ -> "c++--"++
    c. Take care of dealing with "foobar --gnu-args" -> No karma up/down
    d.  One way of handling karma collapse is -> "c++--" -> "c++, Downvote", but "c++----" -> "c++, Downvote", which would allow for "a++--" types   of karma, but would fall apart if we have specific cases where we want/don't want it so may need to have an escape hatch

7. Precinc/dec (++foo) (REJECTED)
    a. gnu style command args
    b. So rarely used

8. Lowercase the outbound karma and nicknames (SEMI-DONE)
    a. Issues on the karmator side, need to track down
    b. casefolding and unicode NFC - Data.Text.ICU

9. Add support for blacklisting certain nick/hosts from interacting with the karmabot

10. Disallow self upvote/downvote/sidevote

11. Identify if submitted by adium (ie adium does -- -> — just downvote adium as such (adium--)) (FEATURE REQUEST)

12. Karma cap
    a. Per channel
    b. Per nick
    c. Floating/self calculating based on historical data/volume

13. Karma events
    a. Enough upvote -> \|/ Karma Fountain \|/
    b. Enough downvote -> Karma Drain


Karmator
========

1. Add support for option parsing for alternative karma command (!karma vs @karma vs...)

2. Fix case lookup (karma/nickname) and do unicode case folding for sql lookup
