# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import pandas as pd

df = pd.read_csv('methods/clean_data/clean_data_srag_hospdeath_epiweek.csv')
df['pos_not_flu_not_sars2'] = df[['VSR',
                                  'PARA1',
                                  'PARA2',
                                  'PARA3',
                                  'PARA4',
                                  'ADNO',
                                  'METAP',
                                  'BOCA',
                                  'RINO']].isin([1]).any(axis=1).astype(int)
df.rename(columns={'pos_not_flu_not_sars2': 'pos_ovr'}, inplace=True)
df['FLU'] = df[['FLU_A',
                'FLU_B',
                'FLU_LAB']].isin([1]).any(axis=1).astype(int)
df = df[(df.DT_SIN_PRI_epiyear < 2020) | (df.DT_SIN_PRI_epiweek <= 22)].copy()

dfgrp = df[['SG_UF_NOT',
            'DT_SIN_PRI_epiyear',
            'FLU',
            'FLU_A',
            'FLU_B',
            'SARS2',
            'TESTED',
            'pos_ovr',
            'VSR',
            'PARA1',
            'PARA2',
            'PARA3',
            'PARA4',
            'ADNO',
            'METAP',
            'BOCA',
            'RINO'
            ]].groupby(by=['SG_UF_NOT', 'DT_SIN_PRI_epiyear'], as_index=False).agg(sum)
tgt_cols = ['FLU',
            'FLU_A',
            'FLU_B',
            'SARS2',
            'pos_ovr',
            'VSR',
            'PARA1',
            'PARA2',
            'PARA3',
            'PARA4',
            'ADNO',
            'METAP',
            'BOCA',
            'RINO'
            ]
dfgrp[tgt_cols] = dfgrp[tgt_cols].divide(dfgrp.TESTED, axis='index')
dfgrp['pool'] = 8
dftmp = dfgrp.copy()
dftmp.pool = 5

dftmp = dfgrp.append(dftmp, ignore_index=True)
dftmp = dftmp[(dftmp.DT_SIN_PRI_epiyear == 2020)].copy()

compdfgrp = (1 - dftmp[tgt_cols]).pow(dftmp.pool, axis='index')
compdfgrp = compdfgrp.join(dftmp[['SG_UF_NOT', 'DT_SIN_PRI_epiyear', 'pool']])
compdfgrp.SG_UF_NOT = compdfgrp.SG_UF_NOT.astype(int)
compdfgrp[tgt_cols] *= 100
compdfgrp[tgt_cols] = compdfgrp[tgt_cols].round(2)
compdfgrp[['SG_UF_NOT', 'pool', 'SARS2', 'FLU', 'pos_ovr']].rename(columns={
    'SG_UF_NOT': 'UF',
    'pos_ovr': 'OVR',
}).to_csv('./analises/prob_pool_negativo_8_5.csv', index=False, sep=';', decimal=',')

dfsamp = pd.DataFrame([{'positividade': x, 'pool': p} for x in range(1, 101, 1) for p in [5, 8, 16]])

import seaborn as sns
import matplotlib.pyplot as plt
dfsamp.prob_pool_neg *= 100
sns.set(context='paper', style='whitegrid')
sns.lineplot(data = dfsamp, x='positividade', y='prob_pool_neg', hue='pool', palette=sns.color_palette("Set1", 3))
plt.ylabel('Prob. de pool negativo (%)')
plt.xlabel('Positividade do vírus de interesse (%)')
plt.xlim([0, 50])
plt.savefig('pooll_negativo_vs_positividade.png', bbox_to_inches='tight')
