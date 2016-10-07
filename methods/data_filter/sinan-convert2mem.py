__author__ = 'Marcelo Ferreira da Costa Gomes'
import pandas as pd
import argparse
from argparse import RawDescriptionHelpFormatter


def readtable(fname, sep=','):

    df = pd.read_csv(fname, sep=sep, low_memory=False)
    df['isoyear'] = pd.DatetimeIndex(df.DT_NOTIFIC).year
    df['isoweek'] = pd.DatetimeIndex(df.DT_NOTIFIC).week
    df = df[['SG_UF_NOT', 'isoweek', 'isoyear']].groupby(['SG_UF_NOT', 'isoyear', 'isoweek'])
    df = df.size().reset_index().rename(columns={0: 'SRAG', 'SG_UF_NOT': 'UF'})
    df.UF = df.UF.astype('int64')

    return(df)


def uf4mem(dfin=pd.DataFrame()):

    df = dfin.copy()

    # Load Population file:
    dfpop = pd.read_csv('../data/serie_2001_2015_TCU_simples.csv')

    # Calculate incidence and structure data in the format accepted by MEM algorithm:
    yearlist = sorted(list(df.isoyear.unique()))
    print(yearlist)
    lastweek = df.isoweek[(df.isoyear == max(yearlist)) & (df.isoweek != 53)].max()
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

    df = dftmp.sort_values(by=['UF', 'isoweek'], axis=0).reset_index().drop('index', axis=1)
    dfinc = dftmpinc.sort_values(by=['UF', 'isoweek'], axis=0).reset_index().drop('index', axis=1)

    return df, dfinc


def region4mem(dfin=pd.DataFrame()):

    df = dfin.copy()

    # Load population size time series:
    dfpop = pd.read_csv('../data/serie_2001_2015_TCU_simples.csv')

    # Load Federal Units aggregation:
    dfreg = pd.read_csv('../data/regioesclimaticas.csv')

    df = pd.merge(df, dfreg[['Código', 'Região']].rename(columns={'Código': 'UF'}))
    df = df.drop('UF', axis=1).groupby(['Região', 'isoyear', 'isoweek'], as_index=False).sum()
    dfBR = df.drop('Região', axis=1).groupby(['isoyear', 'isoweek'], as_index=False).sum()
    dfBR['Região'] = 'BR'
    df = df.append(dfBR, ignore_index=True)

    dfpop = pd.merge(dfpop, dfreg[['Código', 'Região']])

    # Aggregate data by proposed regional subdivision:
    dfpop = dfpop.drop(['Código','Unidades da Federação'], axis=1).groupby('Região', as_index=False).sum()

    # Calculate incidence and structure data in the format accepted by MEM algorithm:
    yearlist = sorted(list(df.isoyear.unique()))
    print(yearlist)
    lastweek = df.isoweek[(df.isoyear == max(yearlist)) & (df.isoweek != 53)].max()
    reglist = list(df['Região'].unique())
    tmpdict = [{'Região': reg, 'isoweek': week} for week in range(1,53) for reg in reglist]
    dftmp = pd.DataFrame(tmpdict)
    dftmpinc = dftmp.copy()
    for year in yearlist:
        lbl = 'SRAG' + str(year)
        dftmp = pd.merge(dftmp, df[df.isoyear == year][['Região', 'isoweek', 'SRAG']].rename(columns={'SRAG': lbl}),
                         on=['Região', 'isoweek'], how='left')
        dftmpinc = pd.merge(dftmpinc, df[df.isoyear == year][['Região', 'isoweek', 'SRAG']].rename(columns={'SRAG': lbl}),
                         on=['Região', 'isoweek'], how='left')
        for reg in reglist:
            dftmpinc.loc[dftmp['Região'] == reg, lbl] *= float(100000)/float(dfpop.loc[dfpop['Região'] == reg, str(year)])
        if year != yearlist[-1]:
            dftmp[lbl] = dftmp[lbl].fillna(0)
            dftmpinc[lbl] = dftmpinc[lbl].fillna(0)
        else:
            dftmp.loc[dftmp.isoweek <= lastweek, lbl] = dftmp.loc[dftmp.isoweek <= lastweek, lbl].fillna(0)
            dftmpinc.loc[dftmpinc.isoweek <= lastweek, lbl] = dftmpinc.loc[dftmpinc.isoweek <= lastweek, lbl].fillna(0)


    df = dftmp.sort_values(by=['Região', 'isoweek'], axis=0).reset_index().drop('index', axis=1)
    dfinc = dftmpinc.sort_values(by=['Região', 'isoweek'], axis=0).reset_index().drop('index', axis=1)

    return df, dfinc


def main(fname, sep=','):

    # Reads and process data:
    df = readtable(fname, sep)

    # UF data:
    df_uf, df_uf_inc = uf4mem(df)

    # Region data:
    df_region, df_region_inc = region4mem(df)
    df_region = df_region.rename(columns={'Região': 'UF'})
    df_region_inc = df_region_inc.rename(columns={'Região': 'UF'})

    # Aggregate datasets
    df = df_uf.append(df_region, ignore_index=True)
    dfinc = df_uf_inc.append(df_region_inc, ignore_index=True)


    # Write output to file:
    fnameout = '.'.join(fname.split('.')[:-1]) + '4mem-incidence.csv'
    dfinc.to_csv(fnameout, index=False)
    fnameout = '.'.join(fname.split('.')[:-1]) + '4mem.csv'
    df.to_csv(fnameout, index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Place each year in a corresponding column, with weeks in [1,52].\n" +
                                     "Aggregate data by regional subdivision given in file regioesclimaticas.csv.\n" +
                                     "Exemple usage:\n" +
                                     "python3 sinan-convert2mem-fmt-regiao.py --path clean_data.csv",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--path', help='Path to data file')
    parser.add_argument('--sep', help='Column separator', default=',')
    args = parser.parse_args()
    print(args)
    main(args.path, args.sep)
