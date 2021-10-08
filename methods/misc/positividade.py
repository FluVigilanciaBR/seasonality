# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

df = pd.read_csv('data/data/clean_data_epiweek-weekly-incidence_w_situation_sragnofever.csv')
dfs = df[['UF',
          'epiyear',
          'epiweek',
          'dado',
          'escala',
          'sexo',
          'SRAG',
          'POSITIVE_CASES',
          'NEGATIVE',
          'INCONCLUSIVE',
          'SARS2',
          'DELAYED',
          'FLU_A',
          'FLU_B',
          'VSR',
          'ADNO',
          'BOCA',
          'RINO',
          'METAP',
          'PARA1',
          'PARA2',
          'PARA3',
          'PARA4',
          'OTHERS'
          ]].copy()

dfs['TESTED'] = dfs.POSITIVE_CASES + dfs.NEGATIVE + dfs.INCONCLUSIVE
dfs['FLU'] = dfs.FLU_A + dfs.FLU_B
dfs['OUTROS'] = dfs[['BOCA',
                     'METAP',
                     'PARA1',
                     'PARA2',
                     'PARA3',
                     'PARA4',
                     'OTHERS'
                     ]].sum(axis=1)
dfs.loc[dfs.epiyear > 2020, 'epiweek'] += 53

dfss = dfs.loc[(dfs.escala == 'casos') &
               (dfs.sexo == 'Total') &
               (dfs.dado.isin(['srag', 'obito'])) &
               (dfs.epiyear >= 2020),
               ['UF',
                'dado',
                'epiweek',
                'SRAG',
                'TESTED',
                'POSITIVE_CASES',
                'DELAYED',
                'FLU',
                'VSR',
                'SARS2',
                'ADNO',
                'RINO',
                'OUTROS']].copy()

dftmp = dfss.copy()
vir_cols = ['FLU', 'VSR', 'SARS2', 'ADNO', 'RINO', 'OUTROS']
dftmp[vir_cols] = dftmp[vir_cols].divide(dftmp.TESTED/100, axis=0)
dftmp['normalizacao'] = 'testados'
dftmp['% de testagem sobre total de casos'] = 100*dftmp.TESTED.divide(dftmp.SRAG, axis=0)
dftmpplt = dftmp.melt(id_vars=['UF', 'epiweek', 'dado', 'normalizacao'], value_vars=vir_cols)

dftmp = dfss.copy()
dftmp[vir_cols] = dftmp[vir_cols].divide(dftmp.POSITIVE_CASES/100, axis=0)
dftmp['normalizacao'] = 'positivos'
dftmpplt = dftmpplt.append(dftmp.melt(id_vars=['UF', 'epiweek', 'dado', 'normalizacao'],
                                      value_vars=vir_cols),
                           ignore_index=True)

dftmpplt.rename(columns={'variable': 'Vírus', 'dado': 'Dado'}, inplace=True)
dftmpplt.loc[dftmpplt.Dado == 'srag', 'Dado'] = 'Casos'
dftmpplt.loc[dftmpplt.Dado == 'obito', 'Dado'] = 'Óbitos'
dfid = pd.read_csv('methods/report/territorios.csv')
dfid.loc[dfid.UF == 'SE', 'sigla'] = 'Região Sudeste'
dfid.loc[dfid.UF == 'S', 'sigla'] = 'Região Sul'
dfid.loc[dfid.UF == 'NE', 'sigla'] = 'Região Nordeste'
dfid.loc[dfid.UF == 'N', 'sigla'] = 'Região Norte'
dfid.loc[dfid.UF == 'CO', 'sigla'] = 'Região Centro-Oeste'


title = {'testados': 'positividade entre os testados',
         'positivos': 'percentual entre os positivos para vírus respiratórios'}
xmax = dftmpplt.epiweek.max()

for uf in dftmpplt.UF.unique():
    for n in ['testados', 'positivos']:
        plt.close('all')
        sigla = dfid.sigla[dfid.UF == str(uf)].values[0]
        ax = sns.lineplot(x='epiweek',
                          y='value',
                          hue='Vírus',
                          style='Dado',
                          data=dftmpplt[(dftmpplt.UF == uf) &
                                        (dftmpplt.normalizacao == n)])

        ax.set_xlabel('Semana epidemiológica de primeiros sintomas', fontfamily='Roboto', fontsize='x-large')
        ax.set_ylabel('Percentual', fontfamily='Roboto', fontsize='x-large')
        plt.xticks(ticks=[1, 8, 16, 24, 32, 40, 48, 54, 62, 70, 78, 86],
                   labels=[1, 8, 16, 24, 32, 40, 48, 1, 8, 16, 24, 32],
                   fontfamily='Roboto', fontsize='large')
        plt.xlim([1, xmax])
        plt.yticks(fontfamily='Roboto', fontsize='large')
        ax.set_title('%s: %s' % (sigla, title[n]), fontfamily='Roboto', fontsize='x-large')
        plt.savefig('methods/misc/positividade/positividade_%s_%s.png' % (uf, n), bbox_to_inches='tight')

for uf in dftmpplt.UF.unique():
    for n in ['testados', 'positivos']:
        plt.close('all')
        sigla = dfid.sigla[dfid.UF == str(uf)].values[0]
        ax = sns.lineplot(x='epiweek',
                          y='value',
                          hue='Vírus',
                          style='Dado',
                          data=dftmpplt[(dftmpplt.UF == uf) &
                                        (dftmpplt.Vírus != 'SARS2') &
                                        (dftmpplt.normalizacao == n)])

        ax.set_xlabel('Semana epidemiológica de primeiros sintomas', fontfamily='Roboto', fontsize='x-large')
        ax.set_ylabel('Percentual', fontfamily='Roboto', fontsize='x-large')
        plt.xticks(ticks=[1, 8, 16, 24, 32, 40, 48, 54, 62, 70, 78, 86],
                   labels=[1, 8, 16, 24, 32, 40, 48, 1, 8, 16, 24, 32],
                   fontfamily='Roboto', fontsize='large')
        plt.xlim([1, xmax])
        plt.yticks(fontfamily='Roboto', fontsize='large')
        ax.set_title('%s: %s' % (sigla, title[n]), fontfamily='Roboto', fontsize='x-large')
        plt.savefig('methods/misc/positividade/positividade_%s_%s_sem_sars2.png' % (uf, n), bbox_to_inches='tight')
