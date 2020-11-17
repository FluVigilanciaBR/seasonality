# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

df = pd.read_csv('../../data/data/clean_data_epiweek-weekly-incidence_w_situation_sragnofever.csv')
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
          'VSR']].copy()
dfs['TESTED'] = dfs.POSITIVE_CASES + dfs.NEGATIVE + dfs.INCONCLUSIVE
dfs['FLU'] = dfs.FLU_A + dfs.FLU_B

dfss = dfs.loc[(dfs.escala == 'casos') &
               (dfs.sexo == 'Total') &
               (dfs.dado.isin(['srag', 'obito'])) &
               (dfs.UF.isin(['BR', 13])) &
               (dfs.epiyear == 2020),
               ['UF',
                'dado',
                'epiweek',
                'SRAG',
                'TESTED',
                'POSITIVE_CASES',
                'DELAYED',
                'FLU',
                'VSR',
                'SARS2']].copy()

dftmp = dfss.copy()
dftmp[['FLU', 'VSR', 'SARS2']] = dftmp[['FLU',
                                        'VSR',
                                        'SARS2']].divide(dftmp.TESTED/100, axis=0)
dftmp['normalizacao'] = 'testados'
dftmp['% de testagem sobre total de casos'] = 100*dftmp.TESTED.divide(dftmp.SRAG, axis=0)
dftmpplt = dftmp.melt(id_vars=['UF', 'epiweek', 'dado', 'normalizacao'], value_vars=['FLU', 'VSR', 'SARS2'])

dftmp = dfss.copy()
dftmp[['FLU', 'VSR', 'SARS2']] = dftmp[['FLU',
                                        'VSR',
                                        'SARS2']].divide(dftmp.POSITIVE_CASES/100, axis=0)
dftmp['normalizacao'] = 'positivos'
dftmpplt = dftmpplt.append(dftmp.melt(id_vars=['UF', 'epiweek', 'dado', 'normalizacao'],
                                      value_vars=['FLU', 'VSR', 'SARS2']),
                           ignore_index=True)

dftmpplt.rename(columns={'variable': 'Vírus', 'dado': 'Dado'}, inplace=True)
dftmpplt.loc[dftmpplt.Dado == 'srag', 'Dado'] = 'Casos'
dftmpplt.loc[dftmpplt.Dado == 'obito', 'Dado'] = 'Óbitos'

ax = sns.lineplot(x='epiweek',
                  y='value',
                  hue='Vírus',
                  style='Dado',
                  data=dftmpplt[(dftmpplt.UF=='BR') &
                                (dftmpplt.normalizacao == 'total')])

ax.set_xlabel('Semana de primeiros sintomas', fontfamily='Roboto', fontsize='x-large')
ax.set_ylabel('Percentual', fontfamily='Roboto', fontsize='x-large')
plt.xticks(fontfamily='Roboto', fontsize='large')
plt.xlim([1,37])
plt.yticks(fontfamily='Roboto', fontsize='large')
ax.set_title('BR: Vírus identificados', fontfamily='Roboto', fontsize='x-large')
plt.savefig('positividade.svg', bbox_to_inches='tight')
