__author__ = 'Marcelo Ferreira da Costa Gomes'
import pandas as pd
import argparse
from argparse import RawDescriptionHelpFormatter

age_cols = ['0-4 anos', '5-9 anos', '10-19 anos', '20-29 anos', '30-39 anos', '40-49 anos',
            '50-59 anos', '60+ anos']

def readtable(fname, sep=','):

    target_col = ['SG_UF_NOT', 'DT_NOTIFIC_epiyear', 'DT_NOTIFIC_epiweek', 'idade_em_anos']
    df = pd.read_csv(fname, sep=sep, low_memory=False)[target_col]
    df['epiyear'] = df.DT_NOTIFIC_epiyear
    df['epiweek'] = df.DT_NOTIFIC_epiweek
    df['0-4 anos'] = (df.idade_em_anos < 5).astype(int)
    df['5-9 anos'] = ((df.idade_em_anos >= 5) & (df.idade_em_anos < 10)).astype(int)
    df['10-19 anos'] = ((df.idade_em_anos >= 10) & (df.idade_em_anos < 20)).astype(int)
    df['20-29 anos'] = ((df.idade_em_anos >= 20) & (df.idade_em_anos < 30)).astype(int)
    df['30-39 anos'] = ((df.idade_em_anos >= 30) & (df.idade_em_anos < 40)).astype(int)
    df['40-49 anos'] = ((df.idade_em_anos >= 40) & (df.idade_em_anos < 50)).astype(int)
    df['50-59 anos'] = ((df.idade_em_anos >= 50) & (df.idade_em_anos < 60)).astype(int)
    df['60+ anos'] = (df.idade_em_anos >= 60).astype(int)

    df.rename(columns={'SG_UF_NOT': 'UF'}, inplace=True)
    grp_cols = ['UF', 'epiyear', 'epiweek'] + age_cols
    df = df[grp_cols].groupby(['UF', 'epiyear', 'epiweek'], as_index=False).agg(sum)

    df['SRAG'] = df[age_cols].apply(sum, axis=1)
    df.UF = df.UF.astype('int64')

    yearlist = sorted(list(df.epiyear.unique()))
    print(yearlist)
    lastweek = df.epiweek[(df.epiyear == max(yearlist)) & (df.epiweek != 53)].max()
    uflist = list(df.UF.unique())
    tmpdict = [{'UF': uf, 'epiyear': year, 'epiweek': week} for week in range(1,53) for year in yearlist[:-1]
               for uf in uflist]
    tmpdict.extend([{'UF': uf, 'epiyear': yearlist[-1], 'epiweek': week} for week in range(1,(lastweek+1))
                    for uf in uflist])
    dftmp = pd.DataFrame(tmpdict)

    dffull = pd.merge(dftmp, df, how='left').fillna(0)

    # Load Federal Units aggregation:
    dfreg = pd.read_csv('../data/regioesclimaticas.csv')

    dffull = pd.merge(dffull, dfreg[['Código', 'Região']].rename(columns={'Código': 'UF'}), how='left')
    dffull_reg = dffull.drop('UF', axis=1).groupby(['Região', 'epiyear', 'epiweek'], as_index=False).sum()
    dfBR = dffull_reg.drop('Região', axis=1).groupby(['epiyear', 'epiweek'], as_index=False).sum()
    dfBR['Região'] = 'BR'
    dffull_reg = dffull_reg.append(dfBR, ignore_index=True).rename(columns={'Região': 'UF'})

    dffull = dffull.drop('Região', axis=1).append(dffull_reg, ignore_index=True)
    dffull = dffull[['UF', 'epiyear', 'epiweek', 'SRAG'] + age_cols]

    return(dffull)


def uf4mem(dfin=pd.DataFrame()):

    df = dfin.copy()

    # Load Population file:
    dfpop = pd.read_csv('../data/PROJECOES_2013_POPULACAO-simples_agebracket.csv')

    # Calculate incidence:
    yearlist = sorted(list(df.epiyear.unique()))
    print(yearlist)
    uflist = list(df.UF.unique())
    dfinc = df.rename(columns={'SRAG': 'Total'})
    tgt_cols = ['Total']+age_cols
    dfpop.set_index('Ano', inplace=True)
    for uf in uflist:
        for year in yearlist:
            tgt_rows = (dfinc.UF == uf) & (dfinc.epiyear == year)
            dfinc.loc[tgt_rows, tgt_cols] = 100000*dfinc.loc[tgt_rows, tgt_cols].\
                div(dfpop.loc[dfpop['Código']==str(uf),tgt_cols].ix[year], axis='columns')
    dfinc.rename(columns={'Total': 'SRAG'}, inplace=True)

    # Calculate incidence and structure data in the format accepted by MEM algorithm:
    lastweek = df.epiweek[(df.epiyear == max(yearlist)) & (df.epiweek != 53)].max()
    tmpdict = [{'UF': uf, 'epiweek': week} for week in range(1,53) for uf in uflist]
    dftmp = pd.DataFrame(tmpdict)
    dftmpinc = dftmp.copy()
    for year in yearlist:
        lbl = 'SRAG' + str(year)
        dftmp = pd.merge(dftmp, df[df.epiyear == year][['UF', 'epiweek', 'SRAG']].rename(columns={'SRAG': lbl}),
                         on=['UF', 'epiweek'], how='left')
        dftmpinc = pd.merge(dftmpinc, dfinc[dfinc.epiyear == year][['UF', 'epiweek', 'SRAG']].rename(columns={'SRAG': lbl}),
                            on=['UF', 'epiweek'], how='left')
        if year != yearlist[-1]:
            dftmp[lbl] = dftmp[lbl].fillna(0)
            dftmpinc[lbl] = dftmpinc[lbl].fillna(0)
        else:
            dftmp.loc[dftmp.epiweek <= lastweek, lbl] = dftmp.loc[dftmp.epiweek <= lastweek, lbl].fillna(0)
            dftmpinc.loc[dftmpinc.epiweek <= lastweek, lbl] = dftmpinc.loc[dftmpinc.epiweek <= lastweek, lbl].fillna(0)

    df4mem = dftmp.sort_values(by=['UF', 'epiweek'], axis=0).reset_index().drop('index', axis=1)
    dfinc4mem = dftmpinc.sort_values(by=['UF', 'epiweek'], axis=0).reset_index().drop('index', axis=1)

    return df, dfinc, df4mem, dfinc4mem


def region4mem(dfin=pd.DataFrame()):

    df = dfin.copy()

    # Load population size time series:
    dfpop = pd.read_csv('../data/PROJECOES_2013_POPULACAO-simples_agebracket.csv')

    # Load Federal Units aggregation:
    dfreg = pd.read_csv('../data/regioesclimaticas.csv')

    df = pd.merge(df, dfreg[['Código', 'Região']].rename(columns={'Código': 'UF'}))
    df = df.drop('UF', axis=1).groupby(['Região', 'epiyear', 'epiweek'], as_index=False).sum()
    dfBR = df.drop('Região', axis=1).groupby(['epiyear', 'epiweek'], as_index=False).sum()
    dfBR['Região'] = 'BR'
    df = df.append(dfBR, ignore_index=True)

    # Calculate incidence and structure data in the format accepted by MEM algorithm:
    yearlist = sorted(list(df.epiyear.unique()))
    print(yearlist)
    lastweek = df.epiweek[(df.epiyear == max(yearlist)) & (df.epiweek != 53)].max()
    reglist = list(df['Região'].unique())
    tmpdict = [{'Região': reg, 'epiweek': week} for week in range(1,53) for reg in reglist]
    dftmp = pd.DataFrame(tmpdict)
    dftmpinc = dftmp.copy()
    for year in yearlist:
        lbl = 'SRAG' + str(year)
        dftmp = pd.merge(dftmp, df[df.epiyear == year][['Região', 'epiweek', 'SRAG']].rename(columns={'SRAG': lbl}),
                         on=['Região', 'epiweek'], how='left')
        dftmpinc = pd.merge(dftmpinc, df[df.epiyear == year][['Região', 'epiweek', 'SRAG']].rename(columns={'SRAG': lbl}),
                         on=['Região', 'epiweek'], how='left')
        for reg in reglist:
            dftmpinc.loc[dftmp['Região'] == reg, lbl] *= float(100000)/float(dfpop.loc[dfpop['Região'] == reg, str(year)])
        if year != yearlist[-1]:
            dftmp[lbl] = dftmp[lbl].fillna(0)
            dftmpinc[lbl] = dftmpinc[lbl].fillna(0)
        else:
            dftmp.loc[dftmp.epiweek <= lastweek, lbl] = dftmp.loc[dftmp.epiweek <= lastweek, lbl].fillna(0)
            dftmpinc.loc[dftmpinc.epiweek <= lastweek, lbl] = dftmpinc.loc[dftmpinc.epiweek <= lastweek, lbl].fillna(0)


    df = dftmp.sort_values(by=['Região', 'epiweek'], axis=0).reset_index().drop('index', axis=1)
    dfinc = dftmpinc.sort_values(by=['Região', 'epiweek'], axis=0).reset_index().drop('index', axis=1)

    return df, dfinc


def main(fname, sep=','):

    # Reads and process data:
    df = readtable(fname, sep)

    # UF data:
    df, dfinc, df4mem, dfinc4mem = uf4mem(df)

    # # Region data:
    # df_region, df_region_inc = region4mem(df)
    # df_region = df_region.rename(columns={'Região': 'UF'})
    # df_region_inc = df_region_inc.rename(columns={'Região': 'UF'})
    #
    # # Aggregate datasets
    # df = df_uf.append(df_region, ignore_index=True)
    # dfinc = df_uf_inc.append(df_region_inc, ignore_index=True)


    # Write output to file:
    fnameout = '.'.join(fname.split('.')[:-1]) + '4mem-incidence.csv'
    dfinc4mem.to_csv(fnameout, index=False)
    fnameout = '.'.join(fname.split('.')[:-1]) + '4mem.csv'
    df4mem.to_csv(fnameout, index=False)

    fnameout = '.'.join(fname.split('.')[:-1]) + '-weekly-incidence.csv'
    dfinc.to_csv(fnameout, index=False)
    fnameout = '.'.join(fname.split('.')[:-1]) + '-weekly.csv'
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
