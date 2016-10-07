__author__ = 'Marcelo Ferreira da Costa Gomes'
import pandas as pd
import argparse
from argparse import RawDescriptionHelpFormatter


def readtable(fname, sep=','):

    dfpop = pd.read_csv('../data/serie_2001_2015_TCU_simples.csv')

    df = pd.read_csv(fname, sep=sep)
    df['isoyear'] = pd.DatetimeIndex(df.DT_NOTIFIC).year
    df['isoweek'] = pd.DatetimeIndex(df.DT_NOTIFIC).week
    df = df[['SG_UF_NOT', 'isoweek', 'isoyear']].groupby(['SG_UF_NOT', 'isoyear', 'isoweek'])
    df = df.size().reset_index().rename(columns={0: 'SRAG', 'SG_UF_NOT': 'UF'})

    yearlist = sorted(list(df.isoyear.unique()))
    print(yearlist)
    lastweek = df.isoweek[(df.isoyear == max(yearlist)) & (df.isoweek != 53)].max()

    df.UF = df.UF.astype('int64')
    uflist = list(df.UF.unique())

    tmpdict = [{'UF': uf, 'isoweek': week} for week in range(1,53) for uf in uflist]
    dftmp = pd.DataFrame(tmpdict)
    dftmpinc = dftmp.copy()
    for year in yearlist:
        lbl = 'SRAG' + str(year)
        dftmp = pd.merge(dftmp, df[df.isoyear == year][['UF', 'isoweek', 'SRAG']].rename(columns={'SRAG': lbl}),
                         on=['UF', 'isoweek'], how='left')
        dftmpinc = pd.merge(dftmpinc, df[df.isoyear == year][['UF', 'isoweek', 'SRAG']].rename(columns={'SRAG': lbl}),
                            on=['UF', 'isoweek'], how='left')
        for uf in uflist:
            dftmpinc.loc[dftmpinc.UF == uf, lbl] *= float(100000)/float(dfpop.loc[dfpop['Código'] == uf, str(year)])
        if year != yearlist[-1]:
            dftmp[lbl] = dftmp[lbl].fillna(0)
            dftmpinc[lbl] = dftmpinc[lbl].fillna(0)
        else:
            dftmp.loc[dftmp.isoweek <= lastweek, lbl] = dftmp.loc[dftmp.isoweek <= lastweek, lbl].fillna(0)
            dftmpinc.loc[dftmpinc.isoweek <= lastweek, lbl] = dftmpinc.loc[dftmpinc.isoweek <= lastweek, lbl].fillna(0)

    df = dftmp.sort(columns=['UF', 'isoweek']).reset_index().drop('index', axis=1)
    dfinc = dftmpinc.sort(columns=['UF', 'isoweek']).reset_index().drop('index', axis=1)

    return df, dfinc


def main(fname, sep=','):

    df, dfinc = readtable(fname, sep)

    fnameout = '.'.join(fname.split('.')[:-1]) + '4mem-incidence.csv'
    dfinc.to_csv(fnameout, index=False)

    fnameout = '.'.join(fname.split('.')[:-1]) + '4mem.csv'
    df.to_csv(fnameout, index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description='Place each year in a corresponding column, with weeks in [1,52]',
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--path', help='Path to data file')
    parser.add_argument('--sep', help='Column separator', default=',')
    args = parser.parse_args()
    print(args)
    main(args.path, args.sep)
