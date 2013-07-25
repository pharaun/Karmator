import datetime

import sqlalchemy as sa
from sqlalchemy.ext.declarative import declarative_base
from twisted.internet.threads import deferToThread


Base = declarative_base()


class Thing(Base):
    __tablename__ = 'things'
    name = sa.Column(sa.String(), primary_key=True)
    thing_type = sa.Column(sa.String(), nullable=False)

    __mapper_args__ = dict(polymorphic_on=thing_type,
                           polymorphic_identity='thing')


class User(Thing):
    __tablename__ = 'users'
    name = sa.Column(sa.String(), sa.ForeignKey(Thing.name), primary_key=True)

    __mapper_args__ = dict(polymorphic_identity='user')


class Vote(Base):
    __tablename__ = 'votes'
    voted_at = sa.Column(sa.DateTime(), nullable=False, primary_key=True,
                         default=datetime.datetime.now)
    by_whom_id = sa.Column(sa.String(), sa.ForeignKey(User.name), nullable=False)
    for_what_id = sa.Column(sa.String(), sa.ForeignKey(Thing.name), nullable=False)
    amount = sa.Column(sa.Integer(), nullable=False)

    by_whom = sa.orm.relationship(User, backref='submitted_votes')
    for_what = sa.orm.relationship(Thing, backref='received_votes')


def connect(engine_string):
    engine = sa.create_engine(engine_string)
    Session = sa.orm.sessionmaker(bind=engine)
    return engine, Session

def interaction(func):
    def wrap(Session, *a, **kw):
        def interactionFunction():
            session = Session()
            try:
                res = func(session, *a, **kw)
            except:
                session.rollback()
                raise
            else:
                session.commit()
                return res
            finally:
                session.close()
        return deferToThread(interactionFunction)
    return wrap
