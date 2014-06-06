#!/bin/bash

path="test-path/"
logs="channel1 channel2"

for arg in $logs; do
	echo "Reloading data for #$arg into karmator"
	time python ./reload_log.py "2014-03-04 05:04" "2014-06-05 18:08" "sqlite:///db.sqlite" "$arg" "$path#$arg.log"
done
