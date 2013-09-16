#!/bin/sh
# ghc --print-libdir
# copy libgmp.a to that directory

cd KarmaParser
ghc --make -Wall -O2 -fforce-recomp karma-parser.hs
cd ../
mv KarmaParser/karma-parser karma-parser
strip karma-parser --strip-unneeded
