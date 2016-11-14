# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import pandas as pd
import argparse
import episem
import datetime
from argparse import RawDescriptionHelpFormatter

age_cols = ['Idade desconhecida', '0-4 anos', '5-9 anos', '10-19 anos', '20-29 anos', '30-39 anos', '40-49 anos',
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

    target_col = ['SG_UF_NOT', 'DT_NOTIFIC_epiyear', 'DT_NOTIFIC_epiweek', 'CS_SEXO', 'idade_em_anos']
    df = pd.read_csv(fname, sep=sep, low_memory=False, encoding='utf-8')[target_col].rename(columns={'CS_SEXO': 'sexo',
                                                                                   'DT_NOTIFIC_epiyear': 'epiyear',
                                                                                   'DT_NOTIFIC_epiweek': 'epiweek'})
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

    # Aggregate independent of sex:
    dftmp = df[grp_cols].groupby(['UF', 'epiyear', 'epiweek'], as_index=False).agg(sum)
    dftmp['SRAG'] = dftmp[age_cols].apply(sum, axis=1)
    dftmp['sexo'] = 'Total'

    # Aggregate separeting by sex:
    grp_cols = ['UF', 'epiyear', 'epiweek', 'sexo'] + age_cols
    df = df[grp_cols].groupby(['UF', 'epiyear', 'epiweek', 'sexo'], as_index=False).agg(sum)
    df['SRAG'] = df[age_cols].apply(sum, axis=1)

    df = df.append(dftmp, ignore_index=True)

    df.UF = df.UF.astype('int64')

    yearlist = sorted(list(df.epiyear.unique()))
    print(yearlist)
    lastweek = df.epiweek[df.epiyear == max(yearlist)].max()
    uflist = list(df.UF.unique())
    sexlist = ['M', 'F', 'I', 'Total']
    tmpdict = []
    for year in yearlist[:-1]:
        for week in range(1, (lastepiweek(year)+1)):
            for uf in uflist:
                tmpdict.extend([{'UF': uf, 'epiyear': year, 'epiweek': week, 'sexo': sex} for sex in sexlist])
    tmpdict.extend([{'UF': uf, 'epiyear': yearlist[-1], 'epiweek': week, 'sexo': sex} for sex in sexlist for week in
                    range(1,(lastweek+1)) for uf in uflist])
    dftmp = pd.DataFrame(tmpdict)

    dffull = pd.merge(dftmp, df, how='left').fillna(0)

    # Load Federal Units aggregation:
    dfreg = pd.read_csv('../data/regioesclimaticas.csv', encoding='utf-8')

    dffull = pd.merge(dffull, dfreg[['Código', 'Região']].rename(columns={'Código': 'UF'}), how='left')
    dffull_reg = dffull.drop('UF', axis=1).groupby(['Região', 'epiyear', 'epiweek', 'sexo'], as_index=False).sum()
    dfBR = dffull_reg.drop('Região', axis=1).groupby(['epiyear', 'epiweek', 'sexo'], as_index=False).sum()
    dfBR['Região'] = 'BR'
    dffull_reg = dffull_reg.append(dfBR, ignore_index=True).rename(columns={'Região': 'UF'})

    dffull = dffull.drop('Região', axis=1).append(dffull_reg, ignore_index=True)
    dffull = dffull[['UF', 'epiyear', 'epiweek', 'sexo', 'SRAG'] + age_cols]

    dffull = dffull.sort_values(by=['UF', 'epiyear', 'epiweek', 'sexo'], axis=0).reset_index().drop('index', axis=1)

    return(dffull)


def uf4mem(dfin=pd.DataFrame()):

    df = dfin.copy()

    # Load Population file:
    dfpop = pd.read_csv('../data/PROJECOES_2013_POPULACAO-simples_v3_agebracket.csv', encoding='utf-8')
    dfpop.rename(columns={'UF': 'Unidade da Federação'}, inplace=True)
    dfpop.rename(columns={'Código': 'UF'}, inplace=True)

    # Calculate incidence:
    yearlist = sorted(list(df.epiyear.unique()))
    uflist = list(df.UF.unique())
    dfinc = df[~(df.sexo == 'I')].rename(columns={'SRAG': 'Total'}).drop(['Idade desconhecida'], axis=1)
    tgt_cols = ['Total'] + age_cols
    tgt_cols.remove('Idade desconhecida')
    dfpop.set_index('Ano', inplace=True)
    for uf in uflist:
        for year in yearlist:
            # Males:
            tgt_rows = (dfinc.UF == uf) & (dfinc.epiyear == year) & (dfinc.sexo == 'M')
            dfpop_tgt_rows = (dfpop.UF==str(uf)) & (dfpop.Sexo == 'M') & (dfpop.index == year)
            dfinc.loc[tgt_rows, tgt_cols] = 100000*dfinc.loc[tgt_rows, tgt_cols].\
                div(dfpop.loc[dfpop_tgt_rows, tgt_cols].ix[year], axis='columns')
            # Females:
            tgt_rows = (dfinc.UF == uf) & (dfinc.epiyear == year) & (dfinc.sexo == 'F')
            dfpop_tgt_rows = (dfpop.UF == str(uf)) & (dfpop.Sexo == 'F') & (dfpop.index == year)
            dfinc.loc[tgt_rows, tgt_cols] = 100000*dfinc.loc[tgt_rows, tgt_cols].\
                div(dfpop.loc[dfpop_tgt_rows, tgt_cols].ix[year], axis='columns')
            # Total:
            tgt_rows = (dfinc.UF == uf) & (dfinc.epiyear == year) & (dfinc.sexo == 'Total')
            dfpop_tgt_rows = (dfpop.UF == str(uf)) & (dfpop.Sexo == 'Total') & (dfpop.index == year)
            dfinc.loc[tgt_rows, tgt_cols] = 100000*dfinc.loc[tgt_rows, tgt_cols].\
                div(dfpop.loc[dfpop_tgt_rows, tgt_cols].ix[year], axis='columns')
    dfinc.rename(columns={'Total': 'SRAG'}, inplace=True)

    # Structure data in the format accepted by MEM algorithm:
    lastweek = df.epiweek[(df.epiyear == max(yearlist)) & (df.epiweek != 53)].max()
    tmpdict = [{'UF': uf, 'epiweek': week} for week in range(1,53) for uf in uflist]
    dftmp = pd.DataFrame(tmpdict)
    dftmpinc = dftmp.copy()
    for year in yearlist:
        lbl = 'SRAG' + str(year)
        dftmp = pd.merge(dftmp, df[(df.epiyear == year) & (df.sexo == 'Total')][['UF', 'epiweek', 'SRAG']].rename(
            columns={'SRAG': lbl}), on=['UF', 'epiweek'], how='left')
        dftmpinc = pd.merge(dftmpinc, dfinc[(df.epiyear == year) & (df.sexo == 'Total')][['UF', 'epiweek',
                                                                                          'SRAG']].rename(columns={'SRAG': lbl}),
                            on=['UF', 'epiweek'], how='left')
        if year != yearlist[-1]:
            dftmp[lbl] = dftmp[lbl].fillna(0)
            dftmpinc[lbl] = dftmpinc[lbl].fillna(0)
        else:
            dftmp.loc[dftmp.epiweek <= lastweek, lbl] = dftmp.loc[dftmp.epiweek <= lastweek, lbl].fillna(0)
            dftmpinc.loc[dftmpinc.epiweek <= lastweek, lbl] = dftmpinc.loc[dftmpinc.epiweek <= lastweek, lbl].fillna(0)

    df4mem = dftmp.sort_values(by=['UF', 'epiweek'], axis=0).reset_index().drop('index', axis=1)
    dfinc4mem = dftmpinc.sort_values(by=['UF', 'epiweek'], axis=0).reset_index().drop('index', axis=1)

    df.UF = df.UF.astype('str')
    dfinc.UF = dfinc.UF.astype('str')
    df = df.merge(dfpop[['UF', 'Unidade da Federação']].drop_duplicates(), how='left')
    dfinc = dfinc.merge(dfpop[['UF', 'Unidade da Federação']].drop_duplicates(), how='left')

    return df, dfinc, df4mem, dfinc4mem


def main(fname, sep=','):

    # Reads and process data:
    df = readtable(fname, sep)

    # Structure data:
    df, dfinc, df4mem, dfinc4mem = uf4mem(df)

    # Write population table to be used for thresholds:
    last_year = int(df.epiyear.max())
    # Load population size time series:
    dfpop = pd.read_csv('../data/PROJECOES_2013_POPULACAO-simples_agebracket.csv', encoding='utf-8')
    dfpopcurrent = dfpop[dfpop.Ano == last_year]
    dfpopcurrent.to_csv('../data/populacao_uf_regional_atual.csv', index=False, encoding='utf-8')

    # Write output to file:
    fnameout = '.'.join(fname.split('.')[:-1]) + '4mem-incidence.csv'
    dfinc4mem.to_csv(fnameout, index=False, encoding='utf-8')
    fnameout = '.'.join(fname.split('.')[:-1]) + '4mem.csv'
    df4mem.to_csv(fnameout, index=False, encoding='utf-8')

    fnameout = '.'.join(fname.split('.')[:-1]) + '-weekly-incidence.csv'
    dfinc['Tipo'] = 'Estado'
    dfinc.loc[dfinc['UF'].isin(['RegN', 'RegL', 'RegC', 'RegS']) ,'Tipo'] = 'Regional'
    dfinc.loc[dfinc['UF'] == 'BR' ,'Tipo'] = 'País'
    dfinc = dfinc.sort_values(by=['UF', 'epiyear', 'epiweek', 'sexo'], axis=0).reset_index().drop('index', axis=1)
    dfinc.to_csv(fnameout, index=False, encoding='utf-8')

    fnameout = '.'.join(fname.split('.')[:-1]) + '-weekly.csv'
    df['Tipo'] = 'Estado'
    df.loc[df['UF'].isin(['RegN', 'RegL', 'RegC', 'RegS']) ,'Tipo'] = 'Regional'
    df.loc[df['UF'] == 'BR' ,'Tipo'] = 'País'
    df = df.sort_values(by=['UF', 'epiyear', 'epiweek', 'sexo'], axis=0).reset_index().drop('index', axis=1)
    df.to_csv(fnameout, index=False, encoding='utf-8')


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
