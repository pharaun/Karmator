#!/usr/bin/env python
import datetime
import random
import time
import string
import sys

import karmabot_models as models


def random_name():
    return ''.join(random.sample(string.ascii_lowercase, 12))


def generate_user(session):
    user = models.User(name='u_' + random_name())
    session.add(user)


def generate_chaff(session):
    user = random.choice(session.query(models.User).all())
    if random.random() >= 0.75:
        thing = models.Thing(name='t_' + random_name())
        session.add(thing)
    else:
        thing = random.choice(session.query(models.Thing).all())
    now = time.time()
    vote = models.Vote(voted_at=datetime.datetime.fromtimestamp(random.random() * now),
                       by_whom=user, for_what=thing, amount=random.choice([-1, 0, 1]))
    session.add(vote)


def main(database_uri, count):
    engine, Session = models.connect(database_uri)
    count = int(count)
    session = Session()
    for x in xrange(count // 100):
        generate_user(session)
    for x in xrange(count):
        generate_chaff(session)
    session.commit()
    session.close()

main(*sys.argv[1:])
