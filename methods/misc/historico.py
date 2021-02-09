# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns


wmax = 48
df = pd.read_csv('data/data/historical_estimated_values_sragnofever.csv')[
    lambda df: (df.epiyear == 2020) &
               (df.escala == 'casos') &
               (df.dado == 'srag')]
dfc = pd.read_csv('data/data/current_estimated_values_sragnofever.csv')[lambda df: (df.epiyear == 2020) &
                                                                                   (df.escala == 'casos') &
                                                                                   (df.dado == 'srag')]
dff = pd.read_csv('methods/clean_data/clean_data_srag_sragnofever_epiweek.csv',
                  low_memory=False).loc[lambda df: df.DT_SIN_PRI_epiyear == 2020,
                                        ['SG_UF_NOT',
                                         'DT_SIN_PRI_epiweek',
                                         'DT_DIGITA_epiweek']].rename(columns={'SG_UF_NOT': 'UF',
                                                                               'DT_SIN_PRI_epiweek': 'epiweek'})

ds = dfc.copy()
ds['rollavg'] = None
wlist = [15, 20, 25, 30, 35, wmax]
uflist = ['BR', '33', '35', '43', '13']


def aggepiweek(dfin, col, w, out_pref='SRAG_'):

    UF_and_col = ['UF'] + [col]
    dftmp = dfin.loc[(dfin.DT_DIGITA_epiweek <= w) &
                     (dfin.epiweek <= w),
                     UF_and_col].groupby(by=UF_and_col).size().reset_index()
    dftmp.rename(columns={col: 'epiweek', 0: '%ss%s' % (out_pref, w)}, inplace=True)
    dftmp.UF = dftmp.UF.astype(int).astype(str)

    dftmpbr = dfin.loc[(dfin.DT_DIGITA_epiweek <= w) &
                       (dfin.epiweek <= w),
                       [col]].groupby(by=[col]).size().reset_index()
    dftmpbr.rename(columns={col: 'epiweek', 0: '%ss%s' % (out_pref, w)}, inplace=True)
    dftmpbr['UF'] = 'BR'

    dftmp = dftmp.append(dftmpbr, ignore_index=True, sort=False)
    dftmp.epiweek = dftmp.epiweek.astype(int)

    return dftmp


for w in wlist:
    # dftmp = dff.loc[(dff.DT_DIGITA_epiweek <= w) &
    #                 (dff.epiweek <= w),
    #                 ['UF',
    #                  'epiweek']].groupby(by=['UF',
    #                                          'epiweek']).size().reset_index()
    # dftmp.rename(columns={0: 'SRAG_%s' % w}, inplace=True)
    # dftmp.UF = dftmp.UF.astype(int).astype(str)
    #
    # dftmpbr = dff.loc[(dff.DT_DIGITA_epiweek <= w) &
    #                   (dff.epiweek <= w),
    #                   ['epiweek']].groupby(by=['epiweek']).size().reset_index()
    # dftmpbr.rename(columns={0: 'SRAG_s%s' % w}, inplace=True)
    # dftmpbr['UF'] = 'BR'

    dftmp = aggepiweek(dff, 'epiweek', w)
    dftmp = dftmp.merge(aggepiweek(dff, 'DT_DIGITA_epiweek', w, 'digita_')[['UF', 'epiweek', 'digita_s%s' % w]],
                        on=['UF', 'epiweek'], how='outer')

    ds = ds.merge(df.loc[(df.base_epiweek == w),
                         ['UF', 'epiweek', '2.5%', '50%', '97.5%']],
                  on=['UF', 'epiweek'],
                  suffixes=('', '_s%s' % w),
                  how='left')
    ds = ds.merge(dftmp,
                  on=['UF', 'epiweek'],
                  how='left')
    ds.loc[ds.epiweek <= w, 'SRAG_s%s' % w] = ds.loc[ds.epiweek <= w, 'SRAG_s%s' % w].fillna(0)
    ds.loc[ds.epiweek <= w, 'digita_s%s' % w] = ds.loc[ds.epiweek <= w, 'digita_s%s' % w].fillna(0)

    ds.loc[pd.isnull(ds['2.5%_s' + str(w)]),
           ['2.5%_s' + str(w),
            '50%_s' + str(w),
            '97.5%_s' + str(w)]] = ds.loc[pd.isnull(ds['2.5%_s' + str(w)]),
                                          'SRAG_s' + str(w)]

wlist = {
    'BR': [35, 30, 25, 20, 15],
    '33': [35, 30, 25, 20, 15],
    '35': [35, 30, 25, 20, 15],
    '43': [35, 30, 25, 20, 15],
    '13': [35, 30, 25, 20, 15]
}
titlelbl = {
    'BR': 'Brasil',
    '33': 'Rio de Janeiro',
    '35': 'São Paulo',
    '43': 'Rio Grande do Sul',
    '13': 'Amazonas'
}
# ds[['SRAG_s30', '50%_s30', '2.5%_s30', '97.5%_s30']] = ds[['SRAG', '50%', '2.5%', '97.5%']]

for uf in uflist:
    fig = plt.figure(figsize=[8, 6], dpi=100)
    ax = fig.add_axes([.1, .08, .88, .85])
    ax.set_prop_cycle(None)
    color_cycle = ax._get_lines.prop_cycler
    for w in wlist[uf]:
        color_shade = next(color_cycle)['color']
        ds[ds.UF == uf].plot(x='epiweek',
                             y='SRAG_s%s' % w,
                             style=':',
                             color=color_shade,
                             lw=2,
                             label='Dado até semana %s' % w,
                             ax=ax)
    ds[ds.UF == uf].plot(x='epiweek',
                         y='SRAG',
                         style='-',
                         color='black',
                         lw=2,
                         ax=ax,
                         label='Dado até semana 36')
    ds[ds.UF == uf].plot(x='epiweek',
                         y='digita_s41',
                         style='--',
                         color='black',
                         lw=2,
                         ax=ax,
                         label='Dado por data de digitação')
    ax.set_xlabel('Semana de primeiros sintomas', fontsize='large')
    ax.set_ylabel('Casos de SRAG', fontsize='large')
    for x in wlist[uf]:
        ax.axvline(x=x, color='lightgrey', ls='--')
    ax.get_legend().remove()
    plt.suptitle('SRAG no %s\n-- dados inseridos até a semana 36 --' % titlelbl[uf], y=.99, fontfamily='Roboto',
                 fontsize='x-large')
    ax.set_xticks([1] + list(range(4, min(wmax+1, 52), 4)))
    ax.set_xticklabels([1] + list(range(4, min(wmax+1, 52), 4)))
    plt.xlim([1, min(wmax+1, 52)])
    plt.xticks(fontfamily='Roboto', fontsize='large')
    plt.yticks(fontfamily='Roboto', fontsize='large')

    plt.savefig('methods/misc/comparacao/comparacao_%s_sinpri_digita.png' % uf)
    plt.close('all')

for uf in uflist:
    fig = plt.figure(figsize=[8, 6], dpi=100)
    ax = fig.add_axes([.1, .08, .88, .85])
    ax.set_prop_cycle(None)
    color_cycle = ax._get_lines.prop_cycler
    for w in wlist[uf]:
        color_shade = next(color_cycle)['color']
        ds[ds.UF == uf].plot(x='epiweek',
                             y='50%_s' + str(w),
                             style='--',
                             color=color_shade,
                             lw=2,
                             label='Estimativa na semana %s' % w,
                             ax=ax)
        ds.loc[ds.UF == uf, 'rollavg'] = ds.loc[ds.UF == uf, '50%_s' + str(w)].rolling(3, center=True).mean()
        ds[ds.UF == uf].plot(x='epiweek',
                             y='rollavg',
                             style='-',
                             color=color_shade,
                             lw=2,
                             label='Média móvel da estimativa',
                             ax=ax)
        ax.fill_between(ds.epiweek[ds.UF == uf],
                        ds.loc[ds.UF == uf, '2.5%_s' + str(w)],
                        ds.loc[ds.UF == uf, '97.5%_s' + str(w)],
                        color=color_shade,
                        alpha=.2,
                        label='IC 95% na semana ' + str(w))
    ds[ds.UF == uf].plot(x='epiweek',
                         y='SRAG',
                         style='-',
                         color='black',
                         lw=2,
                         ax=ax,
                         label='Dado até semana 36')
    ax.set_xlabel('Semana de primeiros sintomas', fontsize='large')
    ax.set_ylabel('Casos de SRAG', fontsize='large')
    for x in wlist[uf]:
        ax.axvline(x=x, color='lightgrey', ls='--')
    ax.get_legend().remove()
    plt.suptitle('SRAG no %s\n-- dados inseridos até a semana 36 --' % titlelbl[uf], y=.99, fontfamily='Roboto',
                 fontsize='x-large')
    ax.set_xticks([1] + list(range(4, min(wmax+1, 52), 4)))
    ax.set_xticklabels([1] + list(range(4, min(wmax+1, 52), 4)))
    plt.xlim([1, min(wmax+1, 52)])
    plt.xticks(fontfamily='Roboto', fontsize='large')
    plt.yticks(fontfamily='Roboto', fontsize='large')

    plt.savefig('methods/misc/comparacao/comparacao_%s_estimado.png' % uf)
    plt.close('all')

for uf in uflist:
    fig = plt.figure(figsize=[8, 6], dpi=100)
    ax = fig.add_axes([.1, .08, .88, .85])
    ax.set_prop_cycle(None)
    color_cycle = ax._get_lines.prop_cycler
    w = wmax
    color_shade = next(color_cycle)['color']
    ds[ds.UF == uf].plot(x='epiweek',
                         y='50%_s' + str(w),
                         style='--',
                         color=color_shade,
                         lw=2,
                         label='Estimativa na semana %s' % w,
                         ax=ax)
    ds.loc[ds.UF == uf, 'rollavg'] = ds.loc[ds.UF == uf, '50%_s' + str(w)].rolling(3, center=True).mean()
    ds[ds.UF == uf].plot(x='epiweek',
                         y='rollavg',
                         style='-',
                         color=color_shade,
                         lw=2,
                         label='Média móvel da estimativa',
                         ax=ax)
    ax.fill_between(ds.epiweek[ds.UF == uf],
                    ds.loc[ds.UF == uf, '2.5%_s' + str(w)],
                    ds.loc[ds.UF == uf, '97.5%_s' + str(w)],
                    color=color_shade,
                    alpha=.2,
                    label='IC 95% na semana ' + str(w))
    ds[ds.UF == uf].plot(x='epiweek',
                         y='SRAG',
                         style='-',
                         color='black',
                         lw=2,
                         ax=ax,
                         label='Dado até semana 36')
    ds[ds.UF == uf].plot(x='epiweek',
                         y='digita_s%s' % w,
                         style='--',
                         color='black',
                         lw=2,
                         ax=ax,
                         label='Dado por data de digitação')
    ax.set_xlabel('Semana de primeiros sintomas', fontsize='large')
    ax.set_ylabel('Casos de SRAG', fontsize='large')
    for x in wlist[uf]:
        ax.axvline(x=x, color='lightgrey', ls='--')
    ax.get_legend().remove()
    plt.suptitle('SRAG no %s\n-- dados inseridos até a semana 36 --' % titlelbl[uf], y=.99, fontfamily='Roboto',
                 fontsize='x-large')
    ax.set_xticks([1] + list(range(4, min(wmax+1, 52), 4)))
    ax.set_xticklabels([1] + list(range(4, min(wmax+1, 52), 4)))
    plt.xlim([1, min(wmax+1, 52)])
    plt.xticks(fontfamily='Roboto', fontsize='large')
    plt.yticks(fontfamily='Roboto', fontsize='large')

    plt.savefig('methods/misc/comparacao/comparacao_%s_estimado_digita.png' % uf)
    plt.close('all')
