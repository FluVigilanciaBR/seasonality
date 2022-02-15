# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

dfcases = pd.read_csv('methods/clean_data/clean_data_srag_sragnofever_epiweek.csv', low_memory=False).loc[
    lambda df: df.DT_SIN_PRI_epiyear == 2020,
    ['SG_UF_NOT',
     'DT_SIN_PRI_epiweek',
     'DT_DIGITA',
     'SinPri2Digita_DelayWeeks']]
dfcases.rename(columns={'SG_UF_NOT': 'UF',
                        'DT_SIN_PRI_epiweek': 'epiweek',
                        'DT_DIGITA': 'digita',
                        'SinPri2Digita_DelayWeeks': 'delay'},
               inplace=True)

dfdelays = dfcases[dfcases.delay <= 5].groupby(by=['UF',
                                                   'epiweek',
                                                   'delay']).size().reset_index()
dfdelays.rename(columns={0: 'SRAG'}, inplace=True)
dfdelays.UF = dfdelays.UF.astype(int).astype(str)

dftmp = dfdelays[['UF', 'epiweek', 'SRAG', 'delay']].groupby(by=['epiweek', 'delay'], as_index=False).agg(sum)
dftmp['UF'] = 'BR'

dfdelays = pd.concat([dfdelays, dftmp], ignore_index=True, sort=False)

dfpop = pd.read_csv('methods/data/populacao_uf_regional_atual.csv')
dfdelays = dfdelays.merge(dfpop[['Código', 'Total']].rename(columns={'Código': 'UF', 'Total': 'populacao'}),
                          on='UF',
                          how='left')
dfdelays.SRAG = 100000*dfdelays.SRAG.divide(dfdelays.populacao)
dfdelays.delay = dfdelays.delay.astype(int)

df = pd.read_csv('methods/clean_data/srag_sragnofever_historical_estimated_incidence.csv')[
    lambda df: df.epiyear == 2020]

dfc = pd.read_csv('methods/clean_data/srag_sragnofever_current_estimated_incidence.csv')[lambda df: df.epiyear ==
                                                                                                    2020]
df0 = df.loc[df.base_epiweek == df.epiweek, ['UF', 'epiweek', '50%', '2.5%', '97.5%']].copy()
df0 = df0.merge(dfdelays.loc[dfdelays.delay == 0, ['UF', 'epiweek', 'SRAG']],
                on=['UF', 'epiweek'],
                how='left').fillna(0, axis=1)
df0['SRAG_d0'] = df0['SRAG']

for i in range(1, 5):
    df0 = df0.merge(dfdelays.loc[dfdelays.delay == i, ['UF', 'epiweek', 'SRAG']],
                    on=['UF', 'epiweek'], suffixes=('', '_d%s' % i), how='left').fillna(0, axis=1)
    df0['SRAG_d%s' % i] += df0['SRAG_d%s' % (i-1)]

df0 = df0.merge(dfc[['UF', 'epiweek', 'SRAG']], how='left', on=['UF', 'epiweek'], suffixes=('', '_c'))
df0 = df0.merge(df.loc[(df.base_epiweek - 1) == df.epiweek, ['UF', 'epiweek', '50%', '2.5%', '97.5%']],
                on=['UF', 'epiweek'], suffixes=('', '_d%s' % 1), how='left')

# Plot
dfid = pd.read_csv('methods/report/territorios.csv')
for uf in df0.UF.unique():
    plt.close('all')
    fig = plt.figure(figsize=[8, 6], dpi=100)
    ax = fig.add_axes([.07,.08,.92,.85])
    df0[df0.UF == uf].plot(x='epiweek',
                           y='SRAG',
                           style=':',
                           ax=ax,
                           lw=2,
                           label='Reportado ao fim da semana')

    d = 1
    df0[df0.UF == uf].plot(x='epiweek',
                           y='SRAG_d%s' % d,
                           style=':',
                           lw=2,
                           ax=ax,
                           label='%s semana depois' %d)
    for i in range(2, 5):
        df0[df0.UF == uf].plot(x='epiweek',
                               y='SRAG_d%s' % i,
                               style=':',
                               ax=ax,
                               label='%s semanas depois' % i,
                               lw=2)

    df0[df0.UF == uf].plot(x='epiweek',
                             y='SRAG_c',
                             style='-',
                             ax=ax,
                             color='black',
                             label='Dado mais recente',
                             lw=2)

    ax.set_prop_cycle(None)
    color_cycle = ax._get_lines.prop_cycler
    color_shade = next(color_cycle)['color']
    df0[df0.UF == uf].plot(x='epiweek',
                             y='50%',
                             style='--',
                             ax=ax,
                             color=color_shade,
                             label='Estimativa ao fim da semana',
                             lw=2)
    ax.fill_between(df0.epiweek[df0.UF == uf],
                    df0.loc[df0.UF == uf, '2.5%'],
                    df0.loc[df0.UF == uf, '97.5%'],
                    color=color_shade, alpha=.2, label='IC 95% ao fim da semana')
    color_shade = next(color_cycle)['color']
    df0[df0.UF == uf].plot(x='epiweek',
                             y='50%_d1',
                             style='--',
                             ax=ax,
                             color=color_shade,
                             label='Estimativa uma semana depois',
                             lw=2)
    ax.fill_between(df0.epiweek[df0.UF == uf],
                    df0.loc[df0.UF == uf, '2.5%_d1'],
                    df0.loc[df0.UF == uf, '97.5%_d1'],
                    color=color_shade, alpha=.2, label='IC 95% uma semana depois')
    ax.set_xlabel('Semana de primeiros sintomas', fontsize='x-large', fontfamily='Roboto')
    ax.set_ylabel('Incidência por 100mil hab.', fontsize='x-large', fontfamily='Roboto')
    ax.legend(loc='upper left',
              fontsize='medium')
    ax.set_xticks([1] + list(range(4, 37, 4)))
    ax.set_xticklabels([1] + list(range(4, 37, 4)))
    plt.xlim([1, 37])
    lbl = dfid.sigla[dfid.UF == uf].values[0]
    plt.suptitle('SRAG no %s\n-- dados inseridos até a semana 36 --' % lbl, y=.99, fontsize='x-large', fontfamily='Roboto')
    plt.xticks(fontfamily='Roboto', fontsize='large')
    plt.yticks(fontfamily='Roboto', fontsize='large')
    plt.savefig('methods/misc/comparacao/comparacao_%s.png' % lbl)
    plt.close('all')

