# coding:utf8

__author__ = 'Marcelo Ferreira da Costa Gomes'
import rpy2.robjects as ro
from numpy import *
from pandas import *
from rpy2.robjects import pandas2ri
from rpy2.robjects.packages import importr

pandas2ri.activate()
import pandas as pd
import numpy as np
import seaborn as sns
import matplotlib.pyplot as plt
import argparse
from argparse import RawDescriptionHelpFormatter
import matplotlib.font_manager as fm
from mpl_toolkits.axes_grid.inset_locator import inset_axes
import matplotlib.ticker as ticker
from scipy.stats.mstats import gmean

# Load R MEM package:
mem = importr('mem')
ro.r.require('mem')
# UF codes
tabela_ufnome = {'11': 'Rondônia',
                 '12': 'Acre',
                 '13': 'Amazonas',
                 '14': 'Roraima',
                 '15': 'Pará',
                 '16': 'Amapá',
                 '17': 'Tocantins',
                 '21': 'Maranhão',
                 '22': 'Piauí',
                 '23': 'Ceará',
                 '24': 'Rio Grande do Norte',
                 '25': 'Paraíba',
                 '26': 'Pernambuco',
                 '27': 'Alagoas',
                 '28': 'Sergipe',
                 '29': 'Bahia',
                 '31': 'Minas Gerais',
                 '32': 'Espírito Santo',
                 '33': 'Rio de Janeiro',
                 '35': 'São Paulo',
                 '41': 'Paraná',
                 '42': 'Santa Catarina',
                 '43': 'Rio Grande do Sul',
                 '50': 'Mato Grosso do Sul',
                 '51': 'Mato Grosso',
                 '52': 'Goiás',
                 '53': 'Distrito Federal',
                 'AfAmN': 'Região 1',
                 'Aw': 'Região 3',
                 'AsBSh': 'Região 2',
                 'Cf': 'Região 4',
                 'BR': 'Brasil'}
tabela_ufcod = {v: k for k, v in tabela_ufnome.items()}
fontproplgd = fm.FontProperties('Oswald')
fontproplgd.set_size(28)
fontproplbl = fm.FontProperties('Oswald')
fontproplbl.set_size(42)
fontproplblinset = fm.FontProperties('Oswald')
fontproplblinset.set_size(30)
fontpropticks = fontproplblinset.copy()
fontpropticks.set_size(24)
fontpropticksinset = fontpropticks.copy()
fontpropticksinset.set_size(20)


def to_bool(v):
    if v in [True, 'T', 't', 'True', 'true', 1, '1']:
        v = True
    else:
        v = False
    return v


def discardseasons(df, seasons, gdthres=2.0, smin=5):
    """
    Calculate peak variability in order to keep only seasons with relatively low variability rdthres.
    Always mantain at least smin seasons.

    :param df: data frame with seasons by columns
    :param seasons: list of column names corresponding to each season
    :param gdthres: maximum geometric deviation from median
    :param smin: minimum number of seasons maintained

    :return drop_seasons: list with seasons to be dropped
    """

    drop_seasons = []
    seasons = seasons.copy()

    # Drop null seasons
    series = df[seasons].max()
    drop_seasons = list(series[series == 0].index)
    series.drop(drop_seasons, axis=0, inplace=True)
    # If resulting data contains less than smin seasons, return
    nseasons = len(series)
    nmax = nseasons - smin
    if nmax <= 0:
        return drop_seasons

    ####### Test removing one by one ######
    # Take log of geometric deviation threshold for simplicity
    gdthres = np.log(gdthres)
    for n in range(nmax):
        # Current maxima
        tmp_series = df[list(set(seasons).difference(drop_seasons))].max()
        # Grab current geometric mean
        series_gmean = np.log(gmean(tmp_series))
        # Calculate maximum geometric deviation from geometric mean
        mgd = abs(np.log(tmp_series) - series_gmean).max()

        if mgd > gdthres:
            idx = abs(np.log(tmp_series) - series_gmean).idxmax()
            drop_seasons.append(idx)

    return drop_seasons


def applymem(df, discarded_seasons=None, wdw_method=2, lower_bound=5.0):
    rdf = pandas2ri.py2ri(df)
    seasons = sorted(list(df.columns.drop(['UF', 'epiweek'])))[:-1]
    # Discard 2009 season if present:
    seasons = sorted(set(seasons).difference(discarded_seasons))
    rseasons = ro.StrVector(seasons)
    ro.globalenv['df'] = rdf
    ro.globalenv['seasons'] = rseasons
    # # Method for obtaining typical time series evolution (default 2)
    # ro.globalenv['par.type.curve'] = 2
    # # Method for obtaining pre/post-epidemic threshold (default 4)
    # ro.globalenv['par.type.threshold'] = 2
    # # Method for obtaining intensity thresholds (default 4)
    # ro.globalenv['par.type.intensity'] = 2
    # # Method for obtaining outbreak start and length (default 6)
    # ro.globalenv['par.type.other'] = 2
    # # Total number of points to obtain pre/post-threshold (will take n/seasons from each)
    # ro.globalenv['par.n.max'] = 30
    # # Confidence interval for modelled curve
    # ro.globalenv['par.level.curve'] = 0.90
    # # Confidence interval for pre/post-thresold
    # ro.globalenv['par.level.threshold'] = 0.95
    # # Quantiles for intensity thresholds
    # ro.globalenv['par.level.intensity'] = ro.FloatVector([0.40, 0.90, 0.975])
    #
    # epimemrslt = ro.r('memmodel(i.data=subset(df, select=seasons), i.type.curve=par.type.curve,' +
    #                   'i.type.threshold=par.type.threshold, i.type.intensity=par.type.intensity,' +
    #                   'i.type.other=par.type.other, i.n.max=par.n.max, i.level.curve=par.level.curve,' +
    #                   'i.level.threshold=par.level.threshold, i.level.intensity=par.level.intensity)')

    ro.globalenv['df'] = rdf
    ro.globalenv['seasons'] = rseasons
    ro.globalenv['par.method'] = wdw_method
    ro.globalenv['par.type.curve'] = 2
    ro.globalenv['par.n.max'] = 20
    ro.globalenv['par.level.curve'] = 0.95
    ro.globalenv['par.level.threshold'] = 0.95
    ro.globalenv['par.type.intensity'] = 6
    ro.globalenv['par.level.intensity'] = ro.FloatVector([0.40, 0.90, 0.975])
    epimemrslt = ro.r('memmodel(i.data=subset(df, select=seasons), i.type.curve=par.type.curve, i.method=par.method,' +
                      'i.n.max=par.n.max, i.level.curve=par.level.curve, i.level.threshold=par.level.threshold,' +
                      'i.type.intensity=par.type.intensity, i.level.intensity=par.level.intensity)')

    # Pre-epidemic threshold:
    epithreshold = max(lower_bound, pandas2ri.ri2py_dataframe(epimemrslt.rx2('pre.post.intervals')).loc[0, 2])
    typrealcurve = pandas2ri.ri2py_dataframe(epimemrslt.rx2('typ.real.curve'))

    # Check for seasons below threshold:
    dropseasons = set()
    for s in seasons:
        if df[s].max() < epithreshold:
            dropseasons.add(s)
    # Drop seasons below threshold and rerun algorithm:
    episeasons = list(seasons)
    if len(dropseasons) > 0 and len(dropseasons) < len(seasons):
        episeasons = sorted(list(set(seasons).difference(dropseasons)))
        ro.globalenv['episeasons'] = ro.StrVector(episeasons)

        # epimemrslt = ro.r('memmodel(i.data=subset(df, select=episeasons), i.type.curve=par.type.curve,' +
        #                   'i.type.threshold=par.type.threshold, i.type.intensity=par.type.intensity,' +
        #                   'i.type.other=par.type.other, i.n.max=par.n.max, i.level.curve=par.level.curve,' +
        #                   'i.level.threshold=par.level.threshold, i.level.intensity=par.level.intensity)')

        epimemrslt = ro.r('memmodel(i.data=subset(df, select=episeasons), i.type.curve=par.type.curve,' +
                          'i.method=par.method,' +
                          'i.n.max=par.n.max, i.level.curve=par.level.curve, i.level.threshold=par.level.threshold,' +
                          'i.type.intensity=par.type.intensity, i.level.intensity=par.level.intensity)')

    # Store results in python dictionary of objects
    pyepimemrslt = {}
    rovector = [ro.vectors.StrVector, ro.vectors.IntVector, ro.vectors.FloatVector, ro.vectors.Vector]
    for name in epimemrslt.names:
        rdata = epimemrslt.rx2(name)
        if name == 'call':
            pyepimemrslt.update({name: str(rdata)})
        elif type(rdata) in rovector:
            pyepimemrslt.update({name: pandas2ri.ri2py_vector(rdata)})
        else:
            pyepimemrslt.update({name: pandas2ri.ri2py_dataframe(rdata)})

    # typ.curve is the typical curve obtained from averaging over epidemic seasons with time rescaled
    # so that the start of the epidemic period coincides with mean.start
    pyepimemrslt['typ.curve'].rename(columns={0: 'baixo', 1: 'mediano', 2: 'alto'}, inplace=True)
    pyepimemrslt['typ.curve']['mediano'].fillna(0, inplace=True)
    pyepimemrslt['typ.curve']['baixo'] = pyepimemrslt['typ.curve']['baixo'].where(
        pyepimemrslt['typ.curve']['baixo'] >= 0,
        other=0)
    pyepimemrslt['typ.curve']['baixo'] = pyepimemrslt['typ.curve']['baixo']. \
        where((-pyepimemrslt['typ.curve']['baixo'].isnull()), other=pyepimemrslt['typ.curve']['mediano'])
    pyepimemrslt['typ.curve']['alto'] = pyepimemrslt['typ.curve']['alto']. \
        where((-pyepimemrslt['typ.curve']['alto'].isnull()), other=pyepimemrslt['typ.curve']['mediano'])

    pyepimemrslt['typ.threshold.curve'].rename(columns={0: 'baixo', 1: 'mediano', 2: 'alto'}, inplace=True)
    pyepimemrslt['typ.threshold.curve']['mediano'].fillna(0, inplace=True)
    pyepimemrslt['typ.threshold.curve']['baixo'] = pyepimemrslt['typ.threshold.curve']['baixo']. \
        where(pyepimemrslt['typ.threshold.curve']['baixo'] >= 0, other=0)
    pyepimemrslt['typ.threshold.curve']['baixo'] = pyepimemrslt['typ.threshold.curve']['baixo']. \
        where((-pyepimemrslt['typ.threshold.curve']['baixo'].isnull()),
              other=pyepimemrslt['typ.threshold.curve']['mediano'])
    pyepimemrslt['typ.threshold.curve']['alto'] = pyepimemrslt['typ.threshold.curve']['alto']. \
        where((-pyepimemrslt['typ.threshold.curve']['alto'].isnull()),
              other=pyepimemrslt['typ.threshold.curve']['mediano'])

    pyepimemrslt['pre.post.intervals'].rename(index={0: 'pre', 1: 'post'}, inplace=True)

    # typ.real.curve is the typical curve without time shift, that is, respecting the original weeks from data
    # this curve is better to keep all seasons, not only the epidemic ones.
    pyepimemrslt['typ.real.curve'] = typrealcurve.copy()
    pyepimemrslt['typ.real.curve'].rename(columns={0: 'baixo', 1: 'mediano', 2: 'alto'}, inplace=True)
    pyepimemrslt['typ.real.curve']['mediano'].fillna(0, inplace=True)
    pyepimemrslt['typ.real.curve'].loc[pyepimemrslt['typ.real.curve']['baixo'] < 0, 'baixo'] = 0
    pyepimemrslt['typ.real.curve']['baixo'] = pyepimemrslt['typ.real.curve']['baixo']. \
        where((-pyepimemrslt['typ.real.curve']['baixo'].isnull()), other=pyepimemrslt['typ.real.curve']['mediano'])
    pyepimemrslt['typ.real.curve']['alto'] = pyepimemrslt['typ.real.curve']['alto']. \
        where((-pyepimemrslt['typ.real.curve']['alto'].isnull()), other=pyepimemrslt['typ.real.curve']['mediano'])
    newcols = {}
    for k, v in enumerate(episeasons):
        newcols[k] = str(v) + ' transladado'
    pyepimemrslt['moving.epidemics'].rename(columns=newcols, inplace=True)

    return pyepimemrslt, dropseasons


def extract_typ_real_curve(df, discarded_seasons=None, wdw_method=2, lower_bound=5.0):

    seasons = sorted(list(df.columns.drop(['UF', 'epiweek'])))[:-1]
    seasons = sorted(set(seasons).difference(discarded_seasons))

    rdf = pandas2ri.py2ri(df)
    rseasons = ro.StrVector(seasons)

    ro.globalenv['df'] = rdf
    ro.globalenv['seasons'] = rseasons
    ro.globalenv['par.method'] = wdw_method
    ro.globalenv['par.type.curve'] = 2
    ro.globalenv['par.level.curve'] = 0.95
    epimemrslt = ro.r('t(apply(subset(df, select=seasons), 1, memci, i.type.curve=par.type.curve, ' +
                      'i.level.curve=par.level.curve))')

    # Pre-epidemic threshold:
    typrealcurve = pd.DataFrame(epimemrslt)

    # Store results in python dictionary of objects
    pyepimemrslt = {}
    # typ.real.curve is the typical curve without time shift, that is, respecting the original weeks from data
    # this curve is better to keep all seasons, not only the epidemic ones.
    pyepimemrslt['typ.real.curve'] = typrealcurve.copy()
    pyepimemrslt['typ.real.curve'].rename(columns={0: 'baixo', 1: 'mediano', 2: 'alto'}, inplace=True)
    pyepimemrslt['typ.real.curve']['mediano'].fillna(0, inplace=True)
    pyepimemrslt['typ.real.curve'].loc[pyepimemrslt['typ.real.curve']['baixo'] < 0, 'baixo'] = 0
    pyepimemrslt['typ.real.curve']['baixo'] = pyepimemrslt['typ.real.curve']['baixo']. \
        where((-pyepimemrslt['typ.real.curve']['baixo'].isnull()), other=pyepimemrslt['typ.real.curve']['mediano'])
    pyepimemrslt['typ.real.curve']['alto'] = pyepimemrslt['typ.real.curve']['alto']. \
        where((-pyepimemrslt['typ.real.curve']['alto'].isnull()), other=pyepimemrslt['typ.real.curve']['mediano'])

    return pyepimemrslt


def plotmemcurve(uf, dftmp, dftmpinset, thresholds, seasons, lastseason, epicols):
    sns.set_style('darkgrid')
    sns.set_context("talk")
    sns.set_palette('Set2', len(seasons) + 4)
    colorcode = sns.color_palette('Set2', len(seasons) + 4)

    fig, ax = plt.subplots(nrows=2, ncols=1, figsize=[20, 20])
    plt.subplots_adjust(hspace=0.3)

    # Set ymax at least = 1:
    maxval1 = dftmp[list(set(seasons).union(['corredor alto', 'intensidade muito alta']).
                         difference(['SRAG2009']))].max().max()
    maxval2 = dftmp[list(set(seasons).union(['curva epi. alta', 'intensidade muito alta']).
                         difference(['SRAG2009']))].max().max()
    if maxval1 < 1:
        ax[0].set_ylim([0, 1])
        ax[1].set_ylim([0, 1])
    else:
        ax[0].set_ylim([0, maxval1])
        ax[1].set_ylim([0, maxval2])
    # if uf == 33:
    #     ax[0].set_ylim([0,0.25])
    # elif uf == 32:
    #     ax[0].set_ylim([0,0.3])

    ax[0].fill_between(dftmp['epiweek'], 0, dftmp['corredor baixo'], color='green', alpha=0.5)
    ax[0].fill_between(dftmp['epiweek'], dftmp['corredor baixo'], dftmp['corredor mediano'], color='yellow',
                       alpha=0.5)
    ax[0].fill_between(dftmp['epiweek'], dftmp['corredor mediano'], dftmp['corredor alto'], color='orange',
                       alpha=0.5)

    dftmp.plot(ax=ax[0], x='epiweek', y=seasons)
    dftmp.plot(ax=ax[0], x='epiweek', y=lastseason, color='k', lw=3)
    dftmp.plot(ax=ax[0], x='epiweek', y='limiar pré-epidêmico', style='--', color='red', alpha=0.8)
    # dftmp.plot(ax=ax[0], x='epiweek', y='intensidade baixa', style='--')
    dftmp.plot(ax=ax[0], x='epiweek', y='intensidade alta', style='--')
    dftmp.plot(ax=ax[0], x='epiweek', y='intensidade muito alta', style='--', color=colorcode[-1])

    # Check for maximum value on y-axis and fill from 'corredor alto' to maxy
    dftmp.plot(ax=ax[0], x='epiweek', y='corredor alto', legend=False, alpha=0)
    miny, maxy = ax[0].get_ylim()
    del (ax[0].lines[-1])
    ax[0].fill_between(dftmp['epiweek'], dftmp['corredor alto'], maxy, color='red', alpha=0.5)
    ax[0].set_ylim([miny, maxy])
    for label in ax[0].get_xticklabels():
        label.set_fontproperties(fontpropticks)
    for label in ax[0].get_yticklabels():
        label.set_fontproperties(fontpropticks)

    #### Start absolute value plot as inset ####
    sns.set_style('whitegrid')
    axinset = inset_axes(ax[0], width='35%', height='35%', loc=1)
    maxval = dftmpinset[list(set(seasons).union([lastseason]).difference(['SRAG2009']))].max().max()
    if maxval < 1:
        axinset.set_ylim([0, 1])
    else:
        axinset.set_ylim([0, maxval])

    dftmpinset.plot(ax=axinset, x='epiweek', y=seasons)
    dftmpinset.plot(ax=axinset, x='epiweek', y=lastseason, color='k', lw=3)
    dftmpinset.plot(ax=axinset, x='epiweek', y='limiar pré-epidêmico absoluto', style='--', color='red', alpha=0.8)
    axinset.legend_.remove()
    axinset.set_xlabel('SE', fontproperties=fontproplblinset)
    axinset.set_ylabel('Casos', fontproperties=fontproplblinset)
    axinset.yaxis.set_major_locator(ticker.MaxNLocator(integer=True))
    for label in axinset.get_xticklabels():
        label.set_fontproperties(fontpropticksinset)
    for label in axinset.get_yticklabels():
        label.set_fontproperties(fontpropticksinset)

    #### Start plot relative to outbreak typical curve ####

    ax[1].fill_between(dftmp['se relativa ao início do surto'], 0, dftmp['curva epi. baixa'], color='green',
                       alpha=0.5)
    ax[1].fill_between(dftmp['se relativa ao início do surto'], dftmp['curva epi. baixa'],
                       dftmp['curva epi. mediana'], color='yellow', alpha=0.5)
    ax[1].fill_between(dftmp['se relativa ao início do surto'], dftmp['curva epi. mediana'],
                       dftmp['curva epi. alta'], color='orange', alpha=0.5)
    dftmp.plot(ax=ax[1], x='se relativa ao início do surto', y='curva epi. mediana', color='silver',
               label='tendência mediana')
    dftmp.plot(ax=ax[1], x='se relativa ao início do surto', y='limiar pré-epidêmico', style='--',
               color='red', alpha=0.8)
    dftmp.plot(ax=ax[1], x='se relativa ao início do surto', y='limiar pós-epidêmico', style='--',
               color='green', alpha=0.5)

    epicolor = []
    for s in epicols:
        s = s.strip(' transladado')
        n = list(seasons).index(s)
        epicolor.append(colorcode[n])
    dftmp.plot(ax=ax[1], x='se relativa ao início do surto', y=epicols, color=epicolor)
    # Check for maximum value on y-axis and fill from 'corredor alto' to maxy
    dftmp.plot(ax=ax[1], x='se relativa ao início do surto', y='curva epi. alta', legend=False, alpha=0)
    miny, maxy = ax[1].get_ylim()
    del (ax[1].lines[-1])
    ax[1].fill_between(dftmp['se relativa ao início do surto'], dftmp['curva epi. alta'], maxy, color='red',
                       alpha=0.5)
    ax[1].set_ylim([miny, maxy])
    ax[1].plot([0, 0], [miny, maxy], '--', color='silver')
    duracao = int(thresholds['mean.length'][0])
    ax[1].plot([duracao, duracao], [miny, maxy], '--', color='silver')

    ax[1].set_title('Tendência ao longo do surto', fontproperties=fontproplbl)
    epistart = int(thresholds['mean.start'][0])
    ax[1].set_xlabel('SE em relação à semana típica de início do surto (SE=%s)' % epistart,
                     fontproperties=fontproplbl)
    minx, maxx = ax[1].get_xlim()
    xticks = sort(np.append(np.arange(0, int(minx), -4), np.arange(4, int(maxx), 4)))
    ax[1].set_xticks(xticks)
    ax[1].set_xticklabels(xticks, fontproperties=fontpropticks)
    for label in ax[0].get_yticklabels():
        label.set_fontproperties(fontpropticks)
    ax[1].set_ylabel('Incidência (por 100mil habitantes)', fontproperties=fontproplbl)
    box = ax[1].get_position()
    ax[1].set_position([box.x0, box.y0, box.width * 0.8, box.height])
    ax[1].legend(prop=fontproplgd, loc='center left', bbox_to_anchor=(1, 0.5))

    ax[0].set_title(tabela_ufnome[uf], fontproperties=fontproplbl)
    ax[0].set_xlabel('SE', fontproperties=fontproplbl)
    ax[0].set_ylabel('Incidência (por 100mil habitantes)', fontproperties=fontproplbl)
    xticks = np.arange(4, 53, 4)
    ax[0].set_xticks(xticks)
    ax[0].set_xticklabels(xticks)
    # Shrink current axis by 10%
    box = ax[0].get_position()
    ax[0].set_position([box.x0, box.y0, box.width * 0.8, box.height])
    ax[0].legend(prop=fontproplgd, loc='center left', bbox_to_anchor=(1, 0.5))

    return fig


def plotmemfailedcurve(uf, dftmp, dftmpinset, seasons, lastseason):
    sns.set_style('darkgrid')
    sns.set_context("talk")
    sns.set_palette('Set2', len(seasons) + 4)
    colorcode = sns.color_palette('Set2', len(seasons) + 4)

    fig, axi = plt.subplots(nrows=1, ncols=1, figsize=[20, 10])
    ax = [axi]

    maxval1 = dftmp[list(set(seasons).union([lastseason]).difference(['SRAG2009']))].max().max()
    if maxval1 < 1:
        ax[0].set_ylim([0, 1])
    else:
        ax[0].set_ylim([0, maxval1])
    # if uf == 33:
    #     ax[0].set_ylim([0,0.25])
    # elif uf == 32:
    #     ax[0].set_ylim([0,0.3])

    dftmp.plot(ax=ax[0], x='epiweek', y=seasons)
    dftmp.plot(ax=ax[0], x='epiweek', y=lastseason, color='k', lw=3)
    dftmp.plot(ax=ax[0], x='epiweek', y='limiar pré-epidêmico', style='--', color='red', alpha=0.8)
    # dftmp.plot(ax=ax[0], x='epiweek', y='intensidade baixa', style='--')
    dftmp.plot(ax=ax[0], x='epiweek', y='intensidade alta', style='--')
    dftmp.plot(ax=ax[0], x='epiweek', y='intensidade muito alta', style='--', color=colorcode[-1])

    for label in ax[0].get_xticklabels():
        label.set_fontproperties(fontpropticks)
    for label in ax[0].get_yticklabels():
        label.set_fontproperties(fontpropticks)

    #### Start absolute value plot as inset ####
    sns.set_style('whitegrid')
    axinset = inset_axes(ax[0], width='35%', height='35%', loc=1)
    maxval = dftmpinset[list(set(seasons).union([lastseason]).difference(['SRAG2009']))].max().max()
    if maxval < 1:
        axinset.set_ylim([0, 1])
    else:
        axinset.set_ylim([0, maxval])

    dftmpinset.plot(ax=axinset, x='epiweek', y=seasons)
    dftmpinset.plot(ax=axinset, x='epiweek', y=lastseason, color='k', lw=3)
    dftmpinset.plot(ax=axinset, x='epiweek', y='limiar pré-epidêmico absoluto', style='--', color='red', alpha=0.8)
    axinset.legend_.remove()
    axinset.set_xlabel('SE', fontproperties=fontproplblinset)
    axinset.set_ylabel('Casos', fontproperties=fontproplblinset)
    axinset.yaxis.set_major_locator(ticker.MaxNLocator(integer=True))
    for label in axinset.get_xticklabels():
        label.set_fontproperties(fontpropticksinset)
    for label in axinset.get_yticklabels():
        label.set_fontproperties(fontpropticksinset)

    ax[0].set_title(tabela_ufnome[uf], fontproperties=fontproplbl)
    ax[0].set_xlabel('SE', fontproperties=fontproplbl)
    ax[0].set_ylabel('Incidência (por 100mil habitantes)', fontproperties=fontproplbl)
    xticks = np.arange(4, 53, 4)
    ax[0].set_xticks(xticks)
    ax[0].set_xticklabels(xticks)
    # Shrink current axis by 10%
    box = ax[0].get_position()
    ax[0].set_position([box.x0, box.y0, box.width * 0.8, box.height])
    ax[0].legend(prop=fontproplgd, loc='center left', bbox_to_anchor=(1, 0.5))

    return fig


def main(fname, plot_curves=False, sep=',', uflist='all'):
    pref = ('.'.join(fname.replace('-incidence', '').split('.')[:-1])).split('/')[-1]
    df = pd.read_csv(fname, sep=sep)
    dfinset = pd.read_csv(fname.replace('-incidence', ''), sep=sep)
    if 'Região' in list(df.columns):
        df.rename(columns={'Região': 'UF'}, inplace=True)
        dfinset.rename(columns={'Região': 'UF'}, inplace=True)

    df.UF = df.UF.astype(str)
    dfinset.UF = dfinset.UF.astype(str)

    plt.interactive(False)
    if uflist == 'all':
        uflist = list(df.UF.unique())

    dfpop = pd.read_csv('../data/populacao_uf_regional_atual.csv')
    dfreport = pd.DataFrame()
    dfcorredor = pd.DataFrame()

    # Define outbreak window method.
    # Check epitiming function from MEM package for detail
    # 1: original method published in Vega et al.
    # 2: second derivative fixed criterium
    wdw_method = 2
    wdw_method_lbl = {1: 'original', 2: 'criterium'}
    for uf in uflist:
        if uf not in list(df.UF.unique()):
            continue
        print(uf)
        dftmp = df[df.UF == uf].reset_index().drop('index', axis=1).copy()
        dftmpinset = dfinset[dfinset.UF == uf].reset_index().drop('index', axis=1).copy()
        seasons = sort(list(dftmp.columns.drop(['UF', 'epiweek'])))
        lastseason = seasons[-1]
        seasons = list(np.delete(seasons, -1))

        # Select "regular seasons" by comparing geometric distance of corresponding peaks
        # discard season 2009 by default
        tmpseasons = seasons.copy()
        tmpseasons.remove('SRAG2009')
        discarded_seasons = discardseasons(df=dftmp, seasons=tmpseasons, gdthres=2.8, smin=4)
        discarded_seasons.extend(['SRAG2009'])

        # Calculate incidence normalization factor, per 100.000
        incidence_norm = np.float(100000 / dfpop.loc[dfpop['Código'] == str(uf), 'Total'])

        lowseasons = set()

        try:
            if dftmpinset[list(set(seasons).difference(discarded_seasons))].max().max() < 3:
                print(uf, 'max < 3')
                raise
            thresholds, lowseasons = applymem(dftmp, discarded_seasons, wdw_method, lower_bound=5 * incidence_norm)
            thresholdsinset, lowseasons = applymem(dftmpinset, discarded_seasons, wdw_method, lower_bound=5)

            if (thresholds['pre.post.intervals'].loc['pre', 2] >= 5 * incidence_norm):
                dftmp['mediana pré-epidêmica'] = thresholds['pre.post.intervals'].loc['pre', 1]
                dftmp['limiar pré-epidêmico'] = thresholds['pre.post.intervals'].loc['pre', 2]
                dftmp['se relativa ao início do surto'] = dftmp['epiweek'] - thresholds['mean.start'][0]
                dftmp['se típica do início do surto'] = thresholds['mean.start'][0]
                # Confidence interval for epi.start
                cimin = thresholds['ci.start'].loc[0, 0]
                cimax = thresholds['ci.start'].loc[0, 2]
                dftmp['IC da se típica do início do surto'] = '[%s-%s]' % (cimin, cimax)
                dftmp['duração típica do surto'] = thresholds['mean.length'][0]
                # Confidence interval for epi.length
                cimin = thresholds['ci.length'].loc[1, 0]
                cimax = thresholds['ci.length'].loc[1, 2]
                dftmp['IC da duração típica do surto'] = '[%s-%s]' % (cimin, cimax)
            else:
                dftmp['mediana pré-epidêmica'] = np.nan
                dftmp['limiar pré-epidêmico'] = 5 * incidence_norm
                dftmp['se relativa ao início do surto'] = np.nan
                dftmp['se típica do início do surto'] = np.nan
                # Confidence interval for epi.start
                cimin = np.nan
                cimax = np.nan
                dftmp['IC da se típica do início do surto'] = np.nan
                dftmp['duração típica do surto'] = np.nan
                # Confidence interval for epi.length
                cimin = np.nan
                cimax = np.nan
                dftmp['IC da duração típica do surto'] = np.nan

            dftmp['limiar pós-epidêmico'] = thresholds['pre.post.intervals'].loc['post', 2]
            dftmp['intensidade baixa'] = thresholds['epi.intervals'].loc[0, 3]
            dftmp['intensidade alta'] = max(10 * incidence_norm, thresholds['epi.intervals'].loc[1, 3])
            dftmp['intensidade muito alta'] = max(20 * incidence_norm, thresholds['epi.intervals'].loc[2, 3])
            dftmp['corredor baixo'] = thresholds['typ.real.curve']['baixo']
            dftmp['corredor mediano'] = thresholds['typ.real.curve']['mediano']
            dftmp['corredor alto'] = thresholds['typ.real.curve']['alto']
            dftmp['População'] = int(dfpop.loc[dfpop['Código'] == str(uf), 'Total'])

            dftmp['curva epi. baixa'] = thresholds['typ.curve']['baixo']
            dftmp['curva epi. mediana'] = thresholds['typ.curve']['mediano']
            dftmp['curva epi. alta'] = thresholds['typ.curve']['alto']
            epicols = list(thresholds['moving.epidemics'].columns)
            dftmp[epicols] = thresholds['moving.epidemics']
            dftmp['n.seasons'] = thresholds['n.seasons'][0]

            # Geometric mean of regular seasons' peak:
            dftmp_peaks = dftmp[seasons].max()
            peak_gmean = gmean(dftmp_peaks[list(set(seasons).difference(discarded_seasons))])
            dftmp_peaks_inset = dftmpinset[seasons].max()
            peak_gmean_inset = gmean(dftmp_peaks_inset[list(set(seasons).difference(discarded_seasons))])
            dftmp['Média geométrica do pico de infecção das temporadas regulares'] = peak_gmean
            dftmpinset['Média geométrica do pico de infecção das temporadas regulares'] = peak_gmean_inset
            for lbl in seasons:
                peak = dftmp_peaks[lbl]
                peak_inset = dftmp_peaks_inset[lbl]
                if peak == 0:
                    textval = '-'
                else:
                    geom_dist = np.log(peak) - np.log(peak_gmean)
                    geom_dist_inset = np.log(peak_inset) - np.log(peak_gmean_inset)
                    textval = '%.2f' % np.e ** abs(geom_dist) + ' vez(es) ' + ("maior" if geom_dist > 0 else "menor")
                    textval_inset = '%.2f' % np.e ** abs(geom_dist_inset) + ' vez(es) ' + ("maior" if geom_dist > 0
                                                                                           else "menor")
                dftmp['Distância geométrica do pico na temporada %s' % lbl] = textval
                dftmpinset['Distância geométrica do pico na temporada %s' % lbl] = textval_inset

            dftmp.to_csv('./mem-data/%s-mem-%s-incidencia-dropgdist%s-droplow%s-%s_method.csv' % (
            pref, tabela_ufnome[uf].replace(' ', '_'),
            '-'.join(discarded_seasons).replace('SRAG', ''),
            '-'.join(lowseasons).replace('SRAG', ''),
            wdw_method_lbl[wdw_method]),
                         index=False)

            dftmpinset['limiar pré-epidêmico absoluto'] = thresholdsinset['pre.post.intervals'].loc['pre', 2]
            if dftmpinset['limiar pré-epidêmico absoluto'].unique() < 5:
                dftmp['limiar pré-epidêmico'] = 5 * incidence_norm
                dftmpinset['limiar pré-epidêmico absoluto'] = 5

            dftmpinset['limiar pós-epidêmico absoluto'] = thresholdsinset['pre.post.intervals'].loc['post', 2]
            if dftmpinset['limiar pós-epidêmico absoluto'].unique() < 5:
                dftmp['limiar pós-epidêmico'] = 5 * incidence_norm
                dftmpinset['limiar pós-epidêmico absoluto'] = 5

            dftmpinset['intensidade baixa absoluto'] = thresholdsinset['epi.intervals'].loc[0, 3]

            dftmpinset['intensidade alta absoluto'] = thresholdsinset['epi.intervals'].loc[1, 3]
            if dftmpinset['intensidade alta absoluto'].unique() < 10:
                dftmp['intensidade alta absoluto'] = 10 * incidence_norm
                dftmpinset['intensidade alta absoluto'] = 10

            dftmpinset['intensidade muito alta absoluto'] = thresholdsinset['epi.intervals'].loc[2, 3]
            if dftmpinset['intensidade muito alta absoluto'].unique() < 20:
                dftmp['intensidade muito alta'] = 20 * incidence_norm
                dftmpinset['intensidade muito alta absoluto'] = 20

            dftmpinset['corredor baixo'] = thresholdsinset['typ.real.curve']['baixo']
            dftmpinset['corredor mediano'] = thresholdsinset['typ.real.curve']['mediano']
            dftmpinset['corredor alto'] = thresholdsinset['typ.real.curve']['alto']
            dftmpinset['se relativa ao início do surto'] = dftmpinset['epiweek'] - thresholdsinset['mean.start'][0]
            dftmpinset['se típica do início do surto'] = thresholdsinset['mean.start'][0]
            dftmpinset['duração típica do surto'] = thresholdsinset['mean.length'][0]
            dftmpinset['curva epi. baixa'] = thresholdsinset['typ.curve']['baixo']
            dftmpinset['curva epi. mediana'] = thresholdsinset['typ.curve']['mediano']
            dftmpinset['curva epi. alta'] = thresholdsinset['typ.curve']['alto']
            epicols = list(thresholdsinset['moving.epidemics'].columns)
            dftmpinset[epicols] = thresholdsinset['moving.epidemics']
            dftmpinset['n.seasons'] = thresholdsinset['n.seasons'][0]
            dftmpinset.to_csv(
                './mem-data/%s-mem-%s-dropgdist%s-droplow%s-%s_method.csv' % (pref, tabela_ufnome[uf].replace(' ', '_'),
                                                                              '-'.join(discarded_seasons).replace(
                                                                                  'SRAG', ''),
                                                                              '-'.join(lowseasons).replace('SRAG', ''),
                                                                              wdw_method_lbl[wdw_method]),
                index=False)

            if (plot_curves == True):
                fig = plotmemcurve(uf=uf, dftmp=dftmp, dftmpinset=dftmpinset, thresholds=thresholds, seasons=seasons,
                                   lastseason=lastseason, epicols=epicols)
                fig.savefig(
                    './mem-data/%s-%s-inset-dropgdist%s-droplow%s-%s_method.svg' %
                    (pref, tabela_ufnome[uf].replace(' ', '_'),
                     '-'.join(discarded_seasons).replace('SRAG', ''),
                     '-'.join(lowseasons).replace('SRAG', ''),
                     wdw_method_lbl[wdw_method]),
                    bbox_inches='tight')
                fig.savefig(
                    './mem-data/%s-%s-inset-dropgdist%s-droplow%s-%s_method.png' %
                    (pref, tabela_ufnome[uf].replace(' ', '_'),
                     '-'.join(discarded_seasons).replace('SRAG', ''),
                     '-'.join(lowseasons).replace('SRAG', ''),
                     wdw_method_lbl[wdw_method]),
                    bbox_inches='tight')
                plt.clf()
                plt.close()


        except:
            print('MEM Failed', uf)
            thresholds = extract_typ_real_curve(dftmp, discarded_seasons, wdw_method,
                                                            lower_bound=5*incidence_norm)
            thresholdsinset = extract_typ_real_curve(dftmpinset, discarded_seasons, wdw_method,
                                                                 lower_bound=5)

            dftmp['mediana pré-epidêmica'] = np.nan
            dftmp['limiar pré-epidêmico'] = 5 * incidence_norm
            dftmp['limiar pós-epidêmico'] = 5 * incidence_norm
            dftmp['intensidade baixa'] = np.nan
            dftmp['intensidade alta'] = 10 * incidence_norm
            dftmp['intensidade muito alta'] = 20 * incidence_norm
            dftmp['corredor baixo'] = thresholds['typ.real.curve']['baixo']
            dftmp['corredor mediano'] = thresholds['typ.real.curve']['mediano']
            dftmp['corredor alto'] = thresholds['typ.real.curve']['alto']
            dftmp['se típica do início do surto'] = np.nan
            dftmp['duração típica do surto'] = np.nan
            dftmp['Média geométrica do pico de infecção das temporadas regulares'] = np.nan
            dftmp['IC da se típica do início do surto'] = np.nan
            dftmp['IC da duração típica do surto'] = np.nan
            dftmp['População'] = int(dfpop.loc[dfpop['Código'] == str(uf), 'Total'])

            dftmp.to_csv('./mem-data/%s-memfailed-%s-dropgdist%s-%s_method.csv' %
                         (pref, tabela_ufnome[uf].replace(' ', '_'),
                          '-'.join(discarded_seasons).replace('SRAG', ''), wdw_method_lbl[wdw_method]), index=False)

            dftmpinset['limiar pré-epidêmico absoluto'] = 5
            dftmpinset['limiar pós-epidêmico absoluto'] = 5
            dftmpinset['intensidade baixa absoluta'] = np.nan
            dftmpinset['intensidade alta absoluta'] = 10
            dftmpinset['intensidade muito alta absoluta'] = 20

            dftmpinset['corredor baixo'] = thresholdsinset['typ.real.curve']['baixo']
            dftmpinset['corredor mediano'] = thresholdsinset['typ.real.curve']['mediano']
            dftmpinset['corredor alto'] = thresholdsinset['typ.real.curve']['alto']
            dftmpinset['se relativa ao início do surto'] = np.nan
            dftmpinset['se típica do início do surto'] = np.nan
            dftmpinset['duração típica do surto'] = np.nan
            dftmpinset['curva epi. baixa'] = np.nan
            dftmpinset['curva epi. mediana'] = np.nan
            dftmpinset['curva epi. alta'] = np.nan
            dftmpinset['n.seasons'] = 0
            dftmpinset.to_csv(
                './mem-data/%s-memfailed-%s-dropgdist%s-%s_method.csv' % (pref, tabela_ufnome[uf].
                                                                                    replace(' ', '_'),
                                                                                    '-'.join(discarded_seasons).replace(
                                                                                        'SRAG', ''),
                                                                                    wdw_method_lbl[wdw_method]),
                index=False)

            if (plot_curves == True):
                fig = plotmemfailedcurve(uf=uf, dftmp=dftmp, dftmpinset=dftmpinset, seasons=seasons,
                                         lastseason=lastseason)
                fig.savefig(
                    './mem-data/%s-%s-inset-dropgdist%s-droplow%s-%s_method.svg' %
                    (pref, tabela_ufnome[uf].replace(' ', '_'),
                     '-'.join(discarded_seasons).replace('SRAG', ''),
                     '-'.join(lowseasons).replace('SRAG', ''),
                     wdw_method_lbl[wdw_method]),
                    bbox_inches='tight')
                fig.savefig(
                    './mem-data/%s-%s-inset-dropgdist%s-droplow%s-%s_method.png' %
                    (pref, tabela_ufnome[uf].replace(' ', '_'),
                     '-'.join(discarded_seasons).replace('SRAG', ''),
                     '-'.join(lowseasons).replace('SRAG', ''),
                     wdw_method_lbl[wdw_method]),
                    bbox_inches='tight')
                plt.clf()
                plt.close()

        dfreport = dfreport.append(dftmp[['UF', 'População', 'Média geométrica do pico de infecção das temporadas '
                                                             'regulares',
                                          'limiar pré-epidêmico', 'intensidade alta', 'intensidade muito alta',
                                          'se típica do início do surto',
                                          'IC da se típica do início do surto', 'duração típica do surto',
                                          'IC da duração típica do surto']].head(1), ignore_index=True)
        dfcorredor = dfcorredor.append(dftmp[['UF', 'População', 'epiweek', 'corredor baixo', 'corredor mediano',
                                              'corredor alto']],
                                       ignore_index=True)

    dfreport['Unidade da Federação'] = dfreport.UF.map(tabela_ufnome)
    dfreport.to_csv('./mem-data/%s-mem-report-%s-method.csv' % (pref, wdw_method_lbl[wdw_method]), index=False)
    dfreport.to_csv('../clean_data/mem-report.csv', index=False)

    dfcorredor['Unidade da Federação'] = dfcorredor.UF.map(tabela_ufnome)
    dfcorredor.to_csv('./mem-data/%s-mem-typical-%s-method.csv' % (pref, wdw_method_lbl[wdw_method]), index=False)
    dfcorredor.to_csv('../clean_data/mem-typical.csv', index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Generate MEM analysis from cleaned SINAN-SRAG data,\n" +
                                                 "for specified Federal Units, if any. If none specified, runs for all.\n" +
                                                 "Example usage:\n" +
                                                 "python3 sinan-mem-inset-thresholds.py --path clean_data4mem-incidence.csv " +
                                                 "--plot False --uflist Aw Cf\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--path', help='Path to data file')
    parser.add_argument('--sep', help='Column separator', default=',')
    parser.add_argument('--plot', help='Plot curves', default=False)
    parser.add_argument('--uflist', nargs='*', default='all')
    args = parser.parse_args()
    args.plot = to_bool(args.plot)
    print(args)
    main(fname=args.path, plot_curves=bool(args.plot), sep=args.sep, uflist=args.uflist)
