from collections import defaultdict
from cStringIO import StringIO
import datetime

import sqlalchemy as sa
import sqlalchemy.orm
import sqlalchemy.exc as exc
from twisted.internet.threads import deferToThread


__hush_pyflakes__ = [sqlalchemy.orm]


metadata = sa.MetaData()


vote = sa.Table(
    'votes', metadata,
    sa.Column('voted_at', sa.String(), nullable=False),
    sa.Column('by_whom_name', sa.String(), nullable=True),
    sa.Column('for_what_name', sa.String(), nullable=False),
    sa.Column('amount', sa.Integer(), nullable=False),
)

vote_amount_map = {
    'Upvote': 1,
    'Sidevote': 0,
    'Downvote': -1,
}

karma_in = sa.Table(
    'karma_received_count', metadata,
    sa.Column('name', sa.String(), nullable=False),
    sa.Column('up', sa.Integer(), nullable=False),
    sa.Column('down', sa.Integer(), nullable=False),
    sa.Column('side', sa.Integer(), nullable=False),
)

karma_out = sa.Table(
    'karma_given_count', metadata,
    sa.Column('name', sa.String(), nullable=False),
    sa.Column('up', sa.Integer(), nullable=False),
    sa.Column('down', sa.Integer(), nullable=False),
    sa.Column('side', sa.Integer(), nullable=False),
)


def connect(engine_string):
    engine = sa.create_engine(engine_string)
    Session = sa.orm.sessionmaker(bind=engine)
    return engine, Session

def interaction(func):
    def wrap(Session, *a, **kw):
        def interactionFunction(session):
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
        if isinstance(Session, sa.orm.session.Session):
            return interactionFunction(Session)
        else:
            return deferToThread(interactionFunction, Session())
    return wrap


def _karma_query(session, column, which):
    karma = sa.func.sum(vote.c.amount).label('karma')
    q = (sa.select([column, karma])
         .group_by(column)
         .order_by(getattr(karma, which)()))
    return q

@interaction
def all_karma_normalized(session):
    q = _karma_query(session, vote.c.for_what_name, 'desc')
    return session.execute(q).fetchall()

def top_n(session, column, count, which):
    q = _karma_query(session, column, which).limit(count)
    return session.execute(q).fetchall()

def top_n_denormalized(session, table, count, which):
    total_karma = table.c.up - table.c.down
    q = (sa.select([table.c.name, total_karma])
         .order_by(getattr(total_karma, which)())
         .limit(count))
    return session.execute(q).fetchall()

@interaction
def all_karma(session):
    q = sa.select([karma_in.c.name, karma_in.c.up - karma_in.c.down])
    return session.execute(q).fetchall()

def ranking(session, column, to_rank):
    subq = (sa.select([sa.func.coalesce(sa.func.sum(vote.c.amount), 0)])
            .where(column == to_rank))
    karma = sa.func.sum(vote.c.amount).label('karma')
    q = (sa.select([None])
         .select_from(vote)
         .group_by(column)
         .having(karma > subq))
    countq = (sa.select([sa.func.count()])
              .select_from(q))
    return session.execute(countq).fetchall()[0][0]

def ranking_denormalized(session, table, to_rank):
    total_karma = table.c.up - table.c.down
    subq = (sa.select([total_karma])
            .where(table.c.name == to_rank))
    q = (sa.select([sa.func.count()])
         .select_from(table)
         .where(total_karma > sa.func.coalesce(subq.as_scalar(), 0)))
    return session.execute(q).fetchall()[0][0]

def count_distinct(session, column, value):
    subq = (sa.select([column])
            .where(column == value))
    q = sa.select([sa.func.count(column.distinct()) + 1 - sa.exists(subq)])
    return session.execute(q).fetchall()[0][0]

def count_table(session, table, value):
    q = (sa.select([sa.func.count() + 1])
         .select_from(table)
         .where(table.name != value))
    return session.execute(q).fetchall()[0][0]

def stats_for(session, table, count, invoker):
    return (
        top_n_denormalized(session, table, count, 'desc'),
        top_n_denormalized(session, table, count, 'asc'),
        ranking_denormalized(session, table, invoker),
        count_table(session, table, invoker),
    )

@interaction
def general_stats(session, count, from_whom):
    return stats_for(session, karma_in, count, from_whom)

@interaction
def giver_stats(session, count, from_whom):
    return stats_for(session, karma_out, count, from_whom)

def top_n_sidevotes_denormalized(session, table, count):
    q = (sa.select([table.c.name, table.c.side])
         .order_by(table.c.side.desc())
         .limit(count))
    return session.execute(q).fetchall()

def sidevote_ranking_denormalized(session, table, to_rank):
    subq = (sa.select([table.c.side])
            .where(table.c.name == to_rank))
    q = (sa.select([sa.func.count()])
         .select_from(table)
         .where(table.c.side > sa.func.coalesce(subq.as_scalar(), 0)))
    return session.execute(q).fetchall()[0][0]

def top_n_sidevotes(session, column, count):
    sidevotes = sa.func.count(vote.c.amount).label('sidevotes')
    q = (sa.select([column, sidevotes])
         .group_by(column)
         .limit(count)
         .where(vote.c.amount == 0)
         .order_by(sidevotes.desc()))
    return session.execute(q).fetchall()

def sidevote_ranking(session, column, to_rank):
    subq = (sa.select([sa.func.count(vote.c.amount)])
            .where(column == to_rank)
            .where(vote.c.amount == 0))
    sidevotes = sa.func.count(vote.c.amount).label('sidevotes')
    q = (sa.select([None])
         .select_from(vote)
         .group_by(column)
         .having(sidevotes > subq))
    countq = (sa.select([sa.func.count()])
              .select_from(q))
    return session.execute(countq).fetchall()[0][0]

@interaction
def sidevote_stats(session, count, from_whom):
    return (
        top_n_sidevotes_denormalized(session, karma_in, count),
        top_n_sidevotes_denormalized(session, karma_out, count),
        sidevote_ranking_denormalized(session, karma_out, from_whom),
        count_table(session, karma_out, from_whom),
        sidevote_ranking_denormalized(session, karma_in, from_whom),
        count_table(session, karma_in, from_whom),
    )

def counts_by_column(session, column, names):
    counts = {}
    q = (sa.select([column, vote.c.amount, sa.func.count(vote.c.amount)])
         .group_by(column, vote.c.amount)
         .where(column.in_(names)))
    for name, amount, count in session.execute(q):
        counts[name, amount] = count
    return [(name, (counts.get((name, amount), 0) for amount in [1, -1, 0]))
            for name in sorted(names)]

@interaction
def all_counts_normalized(session, column):
    counts = defaultdict(int)
    q = (sa.select([column, vote.c.amount, sa.func.sum(vote.c.amount)])
         .group_by(column, vote.c.amount))
    names = set()
    for name, amount, count in session.execute(q):
        counts[name, cmp(amount, 0)] += count
        names.add(name)
    return [(name, [counts.get((name, amount), 0) for amount in [1, -1, 0]])
            for name in sorted(names)]

@interaction
def all_counts(session, table):
    q = sa.select([table.c.name, table.c.up, table.c.down, table.c.side])
    return [(r.name, (r.up, r.down, r.side)) for r in session.execute(q)]

@interaction
def counts(session, names, table=karma_in):
    if not names:
        return []
    q = (sa.select([table.c.name, table.c.up, table.c.down, table.c.side])
         .where(table.c.name.in_(names)))
    ret = {}
    for r in session.execute(q):
        ret[r.name] = r.up, r.down, r.side
    return [(name, ret.get(name, (0, 0, 0))) for name in names]

#@interaction
def counts_normalized(session, names):
    if not names:
        return {}
    return counts_by_column(session, vote.c.for_what_name, names)

#@interaction
def user_counts(session, names):
    if not names:
        return {}
    return counts_by_column(session, vote.c.by_whom_name, names)

#@interaction
def add_karma(session, json_blob):
    by_whom_name = json_blob['nick']
    for kind in json_blob['karma']:
        q = (vote.insert().values(
                voted_at=(datetime.datetime.utcnow().isoformat(' ') + " UTC"),
                by_whom_name=by_whom_name,
                for_what_name=kind['message'],
                amount=vote_amount_map[kind['karma_type']]
            ))

        for attempt in range(10):
            try:
                session.execute(q)
            except exc.OperationalError as e:
                print "retrying %s" % (e,)
            else:
                break

@interaction
def all_counts_graph(session, table, sortkey):
    counts = all_counts(session, table)
    from matplotlib.pyplot import figure
    fig = figure()
    plt = fig.add_subplot(111)
    x, up, down, names = [], [], [], []
    counts.sort(key=sortkey, reverse=True)
    for e, t in enumerate(counts):
        if sortkey(t) < 20:
            continue
        if e < 10:
            names.append(t[0])
        x.append(e)
        up.append(t[1][0])
        down.append(-t[1][1])
    plt.annotate('\n'.join(names), xy=(1, 1), xycoords='figure fraction',
                 xytext=(-20, -20), textcoords='offset points', ha='right', va='top',
                 bbox=dict(boxstyle='round', fc='0.95'))
    plt.set_xlim([x[0], x[-1]])
    plt.fill_between(x, y1=0, y2=up, color='g', alpha=0.8)
    plt.fill_between(x, y1=down, y2=0, color='r', alpha=0.8)
    io = StringIO()
    fig.savefig(io, format='svg')
    return io.getvalue()
