# -*- coding: utf-8 -*-
"""
This is a basic unique-karma.log dumper that opens a subprocess and
pipes the database into the subprocess in json format and then stores the
output also in json format.

This is for ease of testing of the parser subsystem.
"""

import simplejson
import subprocess
import re
import sys

import karmabot_models as models


USER_MESSAGE = re.compile(
    r'^<.?(?P<nick>[a-z0-9_\-\[\]\\^{}|`]*)> (?P<message>.*)$',
    re.IGNORECASE
)

def karma_parser():
    return subprocess.Popen(
        ['./karma-parser'],
        #['runhaskell', 'karma-parser.hs'],
        bufsize=1, # Line buffered
        stdin=subprocess.PIPE,
        stdout=subprocess.PIPE
    )

def communicate(process, json):
    process.stdin.write(json + '\n')
    process.stdin.flush()

    # Wait for it to process and reply
    return process.stdout.readline()

def karma_loader():
    p = karma_parser()
    session = None
    if len(sys.argv) > 2:
        _, Session = models.connect(sys.argv[2])
        session = Session()

    for line in open(sys.argv[1]):
        message = USER_MESSAGE.match(line)

        if message is not None:
            json = simplejson.dumps(message.groupdict())

            # TODO: make this robust to the process crashing, restart it
            output = communicate(p, json)
            if session is not None:
                line_data = simplejson.loads(output)
                if line_data['karma']:
                    models.add_karma(session, line_data)
                    session.flush()
            sys.stdout.write(output)

    if session is not None:
        session.commit()


if __name__ == '__main__':
    karma_loader()
