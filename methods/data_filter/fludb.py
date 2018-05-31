# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'
# local
from settings import DATABASE

import sqlalchemy as sqla


class FluDB:
    conn = None

    def __init__(self):
        dsn = 'postgresql://%(USER)s:%(PASSWORD)s@%(HOST)s/%(NAME)s'
        self.conn = sqla.create_engine(dsn % DATABASE)
