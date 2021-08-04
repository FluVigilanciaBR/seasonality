# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns


wmax = 73

df = pd.read_csv('data/data/historical_estimated_values_sragnofever.csv')[
    lambda df: (df.epiyear >= 2020) &
               (df.escala == 'casos') &
               (df.dado == 'srag')]
dfc = pd.read_csv('data/data/current_estimated_values_sragnofever.csv')[lambda df: (df.epiyear >= 2020) &
                                                                                   (df.escala == 'casos') &
                                                                                   (df.dado == 'srag')]
wmax_lbl = dfc.epiweek[dfc.epiyear == dfc.epiyear.max()].max()

dff = pd.read_csv('methods/clean_data/clean_data_srag_sragnofever_epiweek.csv',
                  low_memory=False).loc[lambda df: df.DT_SIN_PRI_epiyear >= 2020,
                                        ['SG_UF_NOT',
                                         'DT_SIN_PRI_epiweek',
                                         'DT_SIN_PRI_epiyear',
                                         'DT_DIGITA_epiyear',
                                         'DT_DIGITA_epiweek']].rename(columns={'SG_UF_NOT': 'UF',
                                                                               'DT_SIN_PRI_epiweek': 'epiweek'})

df.loc[df.epiyear == 2021, 'epiweek'] += 53
df.loc[df.base_epiyear == 2021, 'base_epiweek'] += 53
dfc.loc[dfc.epiyear == 2021, 'epiweek'] += 53
dff.loc[dff.DT_SIN_PRI_epiyear == 2021, 'epiweek'] += 53
dff.loc[dff.DT_DIGITA_epiyear == 2021, 'DT_DIGITA_epiweek'] += 53
ds = dfc.copy()
ds['rollavg'] = None

wlist = [x for x in range(15, wmax-1, 5)] + [wmax]
uflist = df.UF.unique().tolist()


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

rev_wlist = [x for x in reversed(wlist)]
# wlist = {
#     'BR': rev_wlist,
#     '33': rev_wlist,
#     '35': rev_wlist,
#     '43': rev_wlist,
#     '13': rev_wlist,
#     '53': rev_wlist
# }
wlist = {uf: rev_wlist for uf in uflist}
# titlelbl = {
#     'BR': 'Brasil',
#     '33': 'Rio de Janeiro',
#     '35': 'São Paulo',
#     '43': 'Rio Grande do Sul',
#     '13': 'Amazonas',
#     '53': 'Distrito Federal'
# }

uftable = pd.read_csv('methods/data/populacao_uf_regional_atual.csv')


# ds[['SRAG_s30', '50%_s30', '2.5%_s30', '97.5%_s30']] = ds[['SRAG', '50%', '2.5%', '97.5%']]
xtcks = [1] + [x for x in range(4, 53, 4)] + [54] + [x for x in range(57, wmax+1, 4)]
xlbls = [1] + [x for x in range(4, 53, 4)] + [1] + [x for x in range(4, wmax+1-53, 4)]
for uf in uflist:
    uflbl = uftable.UF[uftable['Código'] == uf].values[0]
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
                         label='Dado até semana %s' % wmax_lbl)
    ds[ds.UF == uf].plot(x='epiweek',
                         y='digita_s%s' % wmax,
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
    # plt.suptitle('SRAG no %s\n-- dados inseridos até a semana %s --' % (titlelbl[uf], wmax_lbl), y=.99,
    #              fontfamily='Roboto',
    #              fontsize='x-large')
    plt.suptitle('SRAG no %s\n-- dados inseridos até a semana %s --' % (uflbl, wmax_lbl),
                 y=.99,
                 fontfamily='Roboto',
                 fontsize='x-large')
    ax.set_xticks(xtcks)
    ax.set_xticklabels(xlbls)
    plt.xlim([1, max(wmax+1, 52)])
    plt.xticks(fontfamily='Roboto', fontsize='large')
    plt.yticks(fontfamily='Roboto', fontsize='large')

    plt.savefig('methods/misc/comparacao/comparacao_%s_sinpri_digita.png' % uf)
    plt.close('all')

for uf in uflist:
    uflbl = uftable.UF[uftable['Código'] == uf].values[0]
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
                         label='Dado até semana %s' % (wmax_lbl))
    ax.set_xlabel('Semana de primeiros sintomas', fontsize='large')
    ax.set_ylabel('Casos de SRAG', fontsize='large')
    for x in wlist[uf]:
        ax.axvline(x=x, color='lightgrey', ls='--')
    ax.get_legend().remove()
    # plt.suptitle('SRAG no %s\n-- dados inseridos até a semana %s --' % (titlelbl[uf], wmax_lbl), y=.99,
    #              fontfamily='Roboto',
    #              fontsize='x-large')
    plt.suptitle('SRAG no %s\n-- dados inseridos até a semana %s --' % (uflbl, wmax_lbl),
                 y=.99,
                 fontfamily='Roboto',
                 fontsize='x-large')
    ax.set_xticks(xtcks)
    ax.set_xticklabels(xlbls)
    plt.xlim([1, max(wmax+1, 52)])
    plt.xticks(fontfamily='Roboto', fontsize='large')
    plt.yticks(fontfamily='Roboto', fontsize='large')

    plt.savefig('methods/misc/comparacao/comparacao_%s_estimado.png' % uf)
    plt.close('all')

for uf in uflist:
    uflbl = uftable.UF[uftable['Código'] == uf].values[0]
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
                         label='Estimativa na semana %s' % wmax_lbl,
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
                         label='Dado até semana %s' % wmax_lbl)
    ds[ds.UF == uf].plot(x='epiweek',
                         y='digita_s%s' % w,
                         style='--',
                         color='black',
                         lw=2,
                         ax=ax,
                         label='Dado por data de digitação')
    ax.set_xlabel('Semana de primeiros sintomas', fontsize='large')
    ax.set_ylabel('Casos de SRAG', fontsize='large')
    # for x in wlist[uf]:
    #     ax.axvline(x=x, color='lightgrey', ls='--')
    # ax.get_legend().remove()
    plt.suptitle('SRAG no %s\n-- dados inseridos até a semana %s --' % (uflbl, wmax_lbl), y=.99,
                 fontfamily='Roboto',
                 fontsize='x-large')
    ax.set_xticks(xtcks)
    ax.set_xticklabels(xlbls)
    plt.xlim([1, max(wmax+1, 52)])
    plt.xticks(fontfamily='Roboto', fontsize='large')
    plt.yticks(fontfamily='Roboto', fontsize='large')

    plt.savefig('methods/misc/comparacao/comparacao_%s_estimado_digita.png' % uf)
    plt.close('all')
