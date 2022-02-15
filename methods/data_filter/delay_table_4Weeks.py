__author__ = 'Marcelo Ferreira da Costa Gomes'

import pandas as pd
import numpy as np
import episem
import sys
import datetime
import calendar
import argparse
from argparse import RawDescriptionHelpFormatter


def readtable(fname):
    tgt_cols = ['SG_UF_NOT', 'DT_SIN_PRI_epiyear', 'DT_SIN_PRI_epiweek', 'SinPri2Digita_DelayWeeks']
    df = pd.read_csv(fname, low_memory=False)[tgt_cols].rename(columns={
        'SG_UF_NOT': 'UF',
        'DT_SIN_PRI_epiyear': 'epiyear',
        'DT_SIN_PRI_epiweek': 'epiweek',
        'SinPri2Digita_DelayWeeks': 'DelayWeeks'
    })
    df = df.loc[~pd.isnull(df.UF), ]
    # df.UF = df.UF.astype('int64')

    return df


def cumhistfull(x):
    xlim = x.max() + 1
    bins = np.arange(-0.5, xlim, 1)
    h, xi = np.histogram(x, bins=bins, normed=True)
    dx = xi[1] - xi[0]
    h = np.cumsum(h) * dx * 100
    dfcumhist = pd.DataFrame([[a, b] for a, b in zip(range(0, min(xlim, 9)), h[0:min(xlim, 9)])],
                             columns=['Weeks', 'Cumulative'])
    if xlim > 9:
        dfcumhist = pd.concat([dfcumhist, pd.DataFrame([{'Weeks': '> 8', 'Cumulative': 100}])],
                              sort=True)
    return dfcumhist


def cumsumdata(x):
    n = len(x)
    f = np.sort(x)
    f = np.array()


def createtable(df=pd.DataFrame(), testmonth=None, testyear=None):
    now = datetime.datetime.now()
    if testmonth is None:
        if testyear is not None:
            exit('Year provided without corresponding month. Please provide (df, month, year) or (df) alone')
        month = now.month
        testyear = now.year
        if month == 1:
            testmonth = 12
            testyear -= 1
        else:
            testmonth = month - 1
    else:
        testmonth = int(testmonth)
        if testyear is None:
            testyear = now.year
    testyear = int(testyear)

    epiweekstart = episem.episem('%s-%s-01' % (testyear, testmonth))
    epiyearstart = int(epiweekstart.split('W')[0])
    epiweekstart = int(epiweekstart.split('W')[1])

    lastday = calendar.monthrange(testyear, testmonth)[1]
    epiweekend = episem.episem('%s-%s-%s' % (testyear, testmonth, lastday))
    epiyearend = int(epiweekend.split('W')[0])
    epiweekend = int(epiweekend.split('W')[1])

    # Filter data keeping only SinPriations within desired window:
    if epiyearend > epiyearstart:
        df = df[((df.epiweek <= epiweekend) & (df.epiyear == epiyearend)) | (df.epiyear < epiyearend)]
        df = df[(df.epiyear == epiyearend) | ((df.epiweek >= epiweekstart) & (df.epiyear == epiyearstart))]
    else:
        df = df[(df.epiyear == epiyearend) & (df.epiweek >= epiweekstart) & (df.epiweek <= epiweekend)]

    # Fill with all epiweeks:
    lastweek = df.epiweek[df.epiyear == epiyearend].max()
    firstweek = df.epiweek[df.epiyear == epiyearstart].min()
    uflist = list(df.UF.unique())
    tmpdict = []
    if epiyearstart == epiyearend:
        for week in range(firstweek, (lastweek + 1)):
            for uf in uflist:
                tmpdict.extend([{'UF': uf, 'epiyear': epiyearend, 'epiweek': week}])
    else:
        year = epiyearstart
        for week in range(firstweek, (int(episem.lastepiweek(year)) + 1)):
            for uf in uflist:
                tmpdict.extend([{'UF': uf, 'epiyear': year, 'epiweek': week}])
        tmpdict.extend([{'UF': uf, 'epiyear': epiyearend, 'epiweek': week} for week in
                        range(1, (lastweek + 1)) for uf in uflist])
    dftmp = pd.DataFrame(tmpdict)

    grp_cols = ['UF', 'epiyear', 'epiweek']
    delay_table = df[grp_cols].groupby(grp_cols, as_index=False).size().reset_index()
    delay_table.rename(columns={0: 'Notifications'}, inplace=True)

    for k in range(0, 27):
        aux = df.loc[df.DelayWeeks == k,].groupby(grp_cols, as_index=False).size().reset_index()
        aux.rename(columns={0: 'd%s' % k}, inplace=True)
        delay_table = delay_table.merge(aux, how='left', on=grp_cols).fillna(0)

    delay_table = dftmp.merge(delay_table, how='left', on=grp_cols).fillna(0)
    delay_table['Notifications_6m'] = delay_table[['d%s' % k for k in range(0, 27)]].sum(axis=1)
    return delay_table, testmonth, testyear


def histograms(df):
    df4w = df[['UF', 'epiyear', 'epiweek', 'd0', 'd1', 'd2', 'd3', 'd4']]
    cols = ['UF', 'd0', 'd1', 'd2', 'd3', 'd4']
    df4w = df4w[cols].groupby('UF', as_index=False).agg(sum)
    cols = ['UF']
    cols.extend(['d%s' % w for w in range(0, 27)])
    df = df[cols].groupby('UF', as_index=False).agg(sum)
    return df4w, df


def main(fname, testmonth=None, testyear=None):
    df = readtable(fname)
    df, testmonth, testyear = createtable(df, testmonth=testmonth, testyear=testyear)
    dfhisto4w, dfhisto = histograms(df)
    df.to_csv(fname[:-4] + '_delay_table_%s_%s.csv' % (testyear, testmonth), index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Exemple usage:\n" +
                                                 "python3 delay_table_4Weeks.py --path \n" +
                                                 "../../data/data/clean_data_epiweek-weekly-incidence_w_situation.csv\n" +
                                                 '--month 1 --year 2017',
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--path', help='Path to data file')
    parser.add_argument('--year', help='Year')
    parser.add_argument('--month', help='Month')
    args = parser.parse_args()
    print(args)
    main(fname=args.path, testmonth=args.month, testyear=args.year)
