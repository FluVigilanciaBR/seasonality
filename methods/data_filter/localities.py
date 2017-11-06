# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import sqlite3
import pandas as pd


conn = sqlite3.connect('../../data/data/infogripe.db')
df = pd.read_csv('../data/PROJECOES_2013_POPULACAO-simples_agebracket.csv').rename(columns={'Código': 'codigo',
                                                                                            'Sigla': 'sigla',
                                                                                            'UF': 'nome'})
df_locations = df[['codigo', 'sigla', 'nome']].drop_duplicates()

cur = conn.cursor()
cur.execute('''create table localidades
               (codigo text primary key not null, sigla text, nome text)''')
conn.commit()

df_locations.to_sql('localidades', conn, if_exists='append', index=False)
conn.close()
