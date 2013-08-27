#!/bin/sh
# ghc --print-libdir
# copy libgmp.a to that directory

cd KarmaParser
ghc -O2 karma-parser.hs
cd ../
mv KarmaParser/karma-parser karma-parser
strip karma-parser --strip-unneeded
