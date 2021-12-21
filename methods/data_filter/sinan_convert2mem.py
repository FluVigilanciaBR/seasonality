# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'


import pandas as pd
import numpy as np
import argparse
import logging
from .episem import lastepiweek
from argparse import RawDescriptionHelpFormatter

module_logger = logging.getLogger('update_system.sinan_convert2mem')
age_cols = ['Idade desconhecida', '0-4 anos', '5-9 anos', '10-19 anos', '20-29 anos', '30-39 anos', '40-49 anos',
            '50-59 anos', '60+ anos']
vir_cols = ['FLU_A',
            'FLU_B',
            'SARS2',
            'VSR',
            'PARA1',
            'PARA2',
            'PARA3',
            'PARA4',
            'ADNO',
            'METAP',
            'BOCA',
            'RINO',
            'OTHERS']

lab_cols_in = vir_cols + ['POSITIVE',
                       'NEGATIVE',
                       'INCONCLUSIVE',
                       'TESTING_IGNORED',
                       'NOTTESTED',
                       'DELAYED']
lab_cols_out = vir_cols + ['POSITIVE_CASES',
                           'NEGATIVE',
                           'INCONCLUSIVE',
                           'TESTING_IGNORED',
                           'NOTTESTED',
                           'DELAYED']


def readtable(fname, sep=','):

    target_col = ['SG_UF_NOT', 'DT_SIN_PRI_epiyearweek', 'DT_SIN_PRI_epiyear', 'DT_SIN_PRI_epiweek', 'CS_SEXO',
                  'idade_em_anos'] + lab_cols_in
    df = pd.read_csv(fname,
                     sep=sep,
                     low_memory=False,
                     encoding='utf-8')[target_col].rename(columns={'CS_SEXO': 'sexo',
                                                                   'DT_SIN_PRI_epiyearweek': 'epiyearweek',
                                                                   'DT_SIN_PRI_epiyear': 'epiyear',
                                                                   'DT_SIN_PRI_epiweek': 'epiweek'})
    df['Idade desconhecida'] = pd.isnull(df.idade_em_anos).astype(int)
    df['< 2 anos'] = (df.idade_em_anos < 2).astype(int)
    df['2-4 anos'] = ((df.idade_em_anos >= 2) & (df.idade_em_anos < 5)).astype(int)
    df['0-4 anos'] = (df.idade_em_anos < 5).astype(int)
    df['5-9 anos'] = ((df.idade_em_anos >= 5) & (df.idade_em_anos < 10)).astype(int)
    df['10-19 anos'] = ((df.idade_em_anos >= 10) & (df.idade_em_anos < 20)).astype(int)
    df['20-29 anos'] = ((df.idade_em_anos >= 20) & (df.idade_em_anos < 30)).astype(int)
    df['30-39 anos'] = ((df.idade_em_anos >= 30) & (df.idade_em_anos < 40)).astype(int)
    df['40-49 anos'] = ((df.idade_em_anos >= 40) & (df.idade_em_anos < 50)).astype(int)
    df['50-59 anos'] = ((df.idade_em_anos >= 50) & (df.idade_em_anos < 60)).astype(int)
    df['60+ anos'] = (df.idade_em_anos >= 60).astype(int)

    tgt_cols = {'Agentes infecciosos detectados': vir_cols,
                'Exames laboratoriais': ['POSITIVE_CASES', 'NEGATIVE', 'INCONCLUSIVE',
                                         'TESTING_IGNORED', 'NOTTESTED', 'DELAYED']}

    # df['POSITIVE_CASES'] = np.logical_not(df['NOTTESTED'] | df['TESTING_IGNORED'] | df['NEGATIVE'] |
    #                                       df['DELAYED'] | df['INCONCLUSIVE']).astype(int)
    df['POSITIVE_CASES'] = (df.POSITIVE == 1).astype(int)

    df.rename(columns={'SG_UF_NOT': 'UF'}, inplace=True)
    grp_cols = ['UF', 'epiyearweek', 'epiyear', 'epiweek', '< 2 anos', '2-4 anos'] + age_cols + \
               tgt_cols['Agentes infecciosos detectados'] + tgt_cols['Exames laboratoriais']

    # Aggregate independent of sex:
    dftmp = df[grp_cols].groupby(['UF', 'epiyearweek', 'epiyear', 'epiweek'], as_index=False).agg(sum)
    dftmp['SRAG'] = dftmp[age_cols].fillna(0).sum(axis=1)
    dftmp['sexo'] = 'Total'

    # Aggregate separating by sex:
    grp_cols = ['UF', 'epiyearweek','epiyear', 'epiweek', 'sexo', '< 2 anos', '2-4 anos'] + age_cols + \
               tgt_cols['Agentes infecciosos detectados'] + tgt_cols['Exames laboratoriais']
    df = df[grp_cols].groupby(['UF', 'epiyearweek', 'epiyear', 'epiweek', 'sexo'], as_index=False).agg(sum)
    df['SRAG'] = df[age_cols].fillna(0).sum(axis=1)

    df = df.append(dftmp, ignore_index=True, sort=True)

    df.UF = df.UF.astype('int64')

    yearlist = sorted(list(df.epiyear.unique()))
    module_logger.info('Year list: %s', yearlist)
    lastweek = df.epiweek[df.epiyear == max(yearlist)].max()
    uflist = list(df.UF.unique())
    sexlist = ['M', 'F', 'I', 'Total']
    tmpdict = []
    for year in yearlist[:-1]:
        for week in range(1, (int(lastepiweek(year))+1)):
            for uf in uflist:
                tmpdict.extend([{'UF': uf, 'epiyearweek': '%sW%02d' % (year, week), 'epiyear': year, 'epiweek': week,
                                 'sexo': sex} for sex in sexlist])
    tmpdict.extend([{'UF': uf, 'epiyearweek': '%sW%02d' % (yearlist[-1], week), 'epiyear': yearlist[-1], 'epiweek': week,
                     'sexo': sex} for sex in sexlist for week in
                    range(1, (lastweek+1)) for uf in uflist])
    dftmp = pd.DataFrame(tmpdict)

    dffull = pd.merge(dftmp, df, how='left').fillna(0)

    # Load Federal Units aggregation:
    dfreg = pd.read_csv('../data/regioesclimaticas.csv', encoding='utf-8')

    dffull = pd.merge(dffull, dfreg[['Código', 'Região', 'Região oficial']].rename(columns={'Código': 'UF'}),
                      how='left')
    dffull_reg = dffull.drop(['UF', 'Região oficial'], axis=1).groupby(['Região', 'epiyearweek', 'epiyear', 'epiweek',
                                                                  'sexo'],
                                                   as_index=False).sum()
    dffull_reg_ofi = dffull.drop(['UF', 'Região'], axis=1).groupby(['Região oficial', 'epiyearweek', 'epiyear',
                                                                  'epiweek', 'sexo'], as_index=False).sum()
    dfBR = dffull.drop(['UF', 'Região', 'Região oficial'], axis=1).groupby(['epiyearweek', 'epiyear', 'epiweek',
                                                                            'sexo'], as_index=False).sum()
    dfBR['Região'] = 'BR'
    dffull_reg.rename(columns={'Região': 'UF'}, inplace=True)
    dfBR.rename(columns={'Região': 'UF'}, inplace=True)
    dffull_reg_ofi.rename(columns={'Região oficial': 'UF'}, inplace=True)
    dffull_reg = dffull_reg.append(dffull_reg_ofi, ignore_index=True, sort=True)
    dffull_reg = dffull_reg.append(dfBR, ignore_index=True, sort=True)

    dffull = dffull.drop(['Região', 'Região oficial'], axis=1).append(dffull_reg, ignore_index=True, sort=True)
    dffull = dffull[['UF', 'epiyearweek', 'epiyear', 'epiweek', 'sexo', 'SRAG', '< 2 anos', '2-4 anos'] + age_cols +
                    tgt_cols['Agentes infecciosos detectados'] + tgt_cols['Exames laboratoriais']]

    dffull = dffull.sort_values(by=['UF', 'epiyearweek', 'epiyear', 'epiweek', 'sexo'],
                                axis=0).reset_index().drop('index', axis=1)

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
    dfinc = df[~(df.sexo == 'I')].rename(columns={'SRAG': 'Total'}).drop(['Idade desconhecida', '< 2 anos', '2-4 anos'],
                                                                         axis=1)
    tgt_cols = ['Total'] + age_cols
    tgt_cols.remove('Idade desconhecida')
    # Incidence from lab results:
    lab_cols = lab_cols_out
    for uf in uflist:
        for year in yearlist:
            for sex in ['M', 'F', 'Total']:
                tgt_rows = (dfinc.UF == uf) & (dfinc.epiyear == year) & (dfinc.sexo == sex)
                dfpop_tgt_rows = (dfpop.UF == str(uf)) & (dfpop.Sexo == sex) & (dfpop.Ano == year)
                # Cases by age:
                dfinc.loc[tgt_rows, tgt_cols] = 100000*dfinc.loc[tgt_rows, tgt_cols].\
                    div(dfpop.loc[np.where(dfpop_tgt_rows)[0], tgt_cols].squeeze())
                # Lab results:
                dfinc.loc[tgt_rows, lab_cols] = (100000*dfinc.loc[tgt_rows, lab_cols] /
                    dfpop.loc[np.where(dfpop_tgt_rows)[0], 'Total'].squeeze())

    dfinc.rename(columns={'Total': 'SRAG'}, inplace=True)

    # Structure data in the format accepted by MEM algorithm:
    lastweek = df.epiweek[(df.epiyear == max(yearlist)) & (df.epiweek != 53)].max()
    tmpdict = [{'UF': uf, 'epiweek': week} for week in range(1, 53) for uf in uflist]
    dftmp = pd.DataFrame(tmpdict)
    dftmpinc = dftmp.copy()
    for year in yearlist:
        lbl = 'SRAG' + str(year)
        dftmp = pd.merge(dftmp, df[(df.epiyear == year) & (df.sexo == 'Total')][['UF', 'epiweek', 'SRAG']].rename(
            columns={'SRAG': lbl}), on=['UF', 'epiweek'], how='left')
        dftmpinc = pd.merge(dftmpinc, dfinc[(dfinc.epiyear == year) & (dfinc.sexo == 'Total')][['UF', 'epiweek',
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
    fnameout = '.'.join(fname.split('.csv')[:-1]) + '4mem-incidence.csv.gz'
    dfinc4mem.to_csv(fnameout, index=False, encoding='utf-8')
    fnameout = '.'.join(fname.split('.csv')[:-1]) + '4mem.csv.gz'
    df4mem.to_csv(fnameout, index=False, encoding='utf-8')

    fnameout = '.'.join(fname.split('.csv')[:-1]) + '-weekly-incidence.csv.gz'
    dfinc['Tipo'] = 'Estado'
    dfinc.loc[dfinc['UF'].isin(['RegN', 'RegL', 'RegC', 'RegS']) ,'Tipo'] = 'Regional'
    dfinc.loc[dfinc['UF'].isin(['N', 'S', 'NE', 'SE', 'CO']), 'Tipo'] = 'Região'
    dfinc.loc[dfinc['UF'] == 'BR' ,'Tipo'] = 'País'
    dfinc = dfinc.sort_values(by=['UF', 'epiyearweek', 'epiyear', 'epiweek', 'sexo'],
                              axis=0).reset_index().drop('index', axis=1)
    dfinc.to_csv(fnameout, index=False, encoding='utf-8')

    fnameout = '.'.join(fname.split('.csv')[:-1]) + '-weekly.csv.gz'
    df['Tipo'] = 'Estado'
    df.loc[df['UF'].isin(['RegN', 'RegL', 'RegC', 'RegS']) ,'Tipo'] = 'Regional'
    df.loc[df['UF'].isin(['N', 'S', 'NE', 'SE', 'CO']), 'Tipo'] = 'Região'
    df.loc[df['UF'] == 'BR' ,'Tipo'] = 'País'
    df = df.sort_values(by=['UF', 'epiyearweek', 'epiyear', 'epiweek', 'sexo'],
                        axis=0).reset_index().drop(['index', '0-4 anos'], axis=1)
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
    main(args.path, args.sep)
