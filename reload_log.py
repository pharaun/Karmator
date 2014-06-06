import datetime
import json
import re
import sys
import subprocess

import karmabot_models as models

# Dumb arg parser
if len(sys.argv) != 6:
    print "reload_log.py <start> <end> <database_uri> <channel> <logfile>"
    print "<start/ends> - semi-iso date time format (2014-03-22 23:33)"
    exit(1)

# Assumed to be local timezone
start = datetime.datetime.strptime(sys.argv[1], '%Y-%m-%d %H:%M')
end   = datetime.datetime.strptime(sys.argv[2], "%Y-%m-%d %H:%M")
database_uri = sys.argv[3]
channel = sys.argv[4]
filename = sys.argv[5]

# Regex compile
# TODO: take in accord --- Log opened Fri Feb 14 11:36:09 2014 
day_change = re.compile(r'^--- Day changed (?P<day>[a-zA-Z]+ [a-zA-Z]+ \d{2} \d{4})')
message    = re.compile(r'^(?P<time>\d{2}:\d{2}) <.(?P<nick>.*?)> (?P<msg>.*)$')

# Database
engine, Session = models.connect(database_uri)

# Parser
karma_parser = subprocess.Popen(['./karma-parser', 'KarmaParser/parser.cfg'], stdin=subprocess.PIPE, stdout=subprocess.PIPE)

# Open the logfile
with open(filename, 'r') as f:

    current_day = None
    for line in f:
        day = day_change.match(line)

        if day is not None:
            # parse and store it in current_day
            new_day = datetime.datetime.strptime(day.group('day'), "%a %b %d %Y")
            if new_day is not None: current_day = new_day.date()

        else:
            # skip if its not more recent than start date or less recent
            # than end date
            if current_day is not None:
                if (start.date() <= current_day) and (current_day <= end.date()):
                    msg = message.match(line)

                    if msg is not None:
                        time = datetime.datetime.strptime(msg.group('time'), "%H:%M")
                        nick = msg.group('nick')
                        mesg = msg.group('msg')

                        if (time is not None) and (not mesg.startswith('!')):
                            timestamp = datetime.datetime.combine(current_day, time.time())
                            # timestamp.replace(tzinfo=None)

                            item = {
                                'user': "unknown",
                                'nick': nick.decode('utf-8'),
                                'channel': channel.decode('utf-8'),
                                'message': mesg.decode('utf-8'),
                            }

                            # pump into parser
                            karma_parser.stdin.write(json.dumps(item) + "\n")
                            karma_parser.stdin.flush()

                            # Read stdout
                            reply = json.loads(karma_parser.stdout.readline())

                            if reply is not None:
                                if reply.get('karma') is not None:
                                    print str(timestamp), str(reply)
                                    # models.add_karma(Session, line_data)

# close graceful
karma_parser.stdin.close()
karma_parser.stdout.close()
karma_parser.terminate()
