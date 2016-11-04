# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import pandas as pd
import argparse
import episem
import datetime
from argparse import RawDescriptionHelpFormatter

age_cols = ['0-4 anos', '5-9 anos', '10-19 anos', '20-29 anos', '30-39 anos', '40-49 anos',
            '50-59 anos', '60+ anos']


def lastepiweek(year):
    # Calcula o valor da última semana do ano

    day = datetime.datetime(int(year), 12, 31)  # Ultimo dia do ano
    day_week = episem.extractweekday(day)  # dia semana do ultimo dia

    if day_week < 3:
        day = day - datetime.timedelta(days=(day_week+1))
    else:
        day = day + datetime.timedelta(days=(6-day_week))

    return(int(episem.episem(day, out='W')))


def readtable(fname, sep=','):

    target_col = ['SG_UF_NOT', 'DT_NOTIFIC_epiyear', 'DT_NOTIFIC_epiweek', 'idade_em_anos']
    df = pd.read_csv(fname, sep=sep, low_memory=False)[target_col]
    df['epiyear'] = df.DT_NOTIFIC_epiyear
    df['epiweek'] = df.DT_NOTIFIC_epiweek
    df['Idade desconhecida'] = pd.isnull(df.idade_em_anos).astype(int)
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
    lastweek = df.epiweek[df.epiyear == max(yearlist)].max()
    uflist = list(df.UF.unique())
    tmpdict = []
    for year in yearlist[:-1]:
        for week in range(1, (lastepiweek(year)+1)):
            for uf in uflist:
                tmpdict.append({'UF': uf, 'epiyear': year, 'epiweek': week})
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


def main(fname, sep=','):

    # Reads and process data:
    df = readtable(fname, sep)

    # Structure data:
    df, dfinc, df4mem, dfinc4mem = uf4mem(df)

    # Write population table to be used for thresholds:
    last_year = int(df.epiyear.max())
    # Load population size time series:
    dfpop = pd.read_csv('../data/PROJECOES_2013_POPULACAO-simples_agebracket.csv')
    dfpopcurrent = dfpop[dfpop.Ano == last_year]
    dfpopcurrent.to_csv('../data/populacao_uf_regional_atual.csv', index=False)

    # Write output to file:
    fnameout = '.'.join(fname.split('.')[:-1]) + '4mem-incidence.csv'
    dfinc4mem.to_csv(fnameout, index=False)
    fnameout = '.'.join(fname.split('.')[:-1]) + '4mem.csv'
    df4mem.to_csv(fnameout, index=False)

    fnameout = '.'.join(fname.split('.')[:-1]) + '-weekly-incidence.csv'
    dfinc['Tipo'] = 'Estado'
    dfinc.loc[dfinc['UF'].isin(['RegN', 'RegL', 'RegC', 'RegS']) ,'Tipo'] = 'Regional'
    dfinc.loc[dfinc['UF'] == 'BR' ,'Tipo'] = 'País'
    dfinc = dfinc.sort_values(by=['UF', 'epiyear', 'epiweek'], axis=0).reset_index().drop('index', axis=1)
    dfinc.to_csv(fnameout, index=False)

    fnameout = '.'.join(fname.split('.')[:-1]) + '-weekly.csv'
    df['Tipo'] = 'Estado'
    df.loc[df['UF'].isin(['RegN', 'RegL', 'RegC', 'RegS']) ,'Tipo'] = 'Regional'
    df.loc[df['UF'] == 'BR' ,'Tipo'] = 'País'
    df = df.sort_values(by=['UF', 'epiyear', 'epiweek'], axis=0).reset_index().drop('index', axis=1)
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
