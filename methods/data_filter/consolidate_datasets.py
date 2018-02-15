# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import sqlite3
import pandas as pd
import argparse
from argparse import RawDescriptionHelpFormatter

basedir = '../clean_data/'


def mergedata_scale(df, df_cases):
    df['escala'] = 'incidência'
    df_cases['escala'] = 'casos'

    df = df.append(df_cases, ignore_index=True)

    return df


def convert_estimates(df, dfpop):
    df_cases = df.copy()

    tgt_cols = ['SRAG', 'mean', '50%', '2.5%',  '97.5%']
    for uf in df_cases.UF.unique():
        df_cases_slice = df_cases[df_cases.UF == uf].copy()
        dfpop_slice = dfpop[(dfpop.UF == uf)]
        for year in df_cases_slice.epiyear.unique():
            df_cases_slice.loc[(df_cases_slice.epiyear == year), tgt_cols] *= dfpop_slice.loc[dfpop_slice.Ano == year,
                                                                                  'Total'].values[0]/100000

        df_cases[df_cases.UF == uf] = df_cases_slice

    df = mergedata_scale(df, df_cases)
    return df


def convert_report(pref):
    # Files mem-report
    df = pd.read_csv(basedir + '%s_mem-report.csv' % pref, encoding='utf-8', low_memory=False)
    df['dado'] = pref
    df_cases = df.copy()
    tgt_cols = ['Média geométrica do pico de infecção das temporadas regulares',
                'limiar pré-epidêmico',
                'intensidade alta',
                'intensidade muito alta']
    for col in tgt_cols:
        df_cases[col] *= df_cases['População']/100000

    df = mergedata_scale(df, df_cases)
    return df


def convert_typical(pref):
    # Files mem-typical
    df = pd.read_csv(basedir + '%s_mem-typical.csv' % pref, encoding='utf-8', low_memory=False)
    df['dado'] = pref
    df_cases = df.copy()
    tgt_cols = ['corredor baixo', 'corredor mediano', 'corredor alto']
    for col in tgt_cols:
        df_cases[col] *= df_cases['População']/100000

    df = mergedata_scale(df, df_cases)
    return df


def clean_data_merge(pref):
    df = pd.read_csv(basedir + 'clean_data_%s_epiweek-weekly-incidence_w_situation.csv' % pref, encoding='utf-8',
                     low_memory=False)
    df['dado'] = pref
    df_cases = pd.read_csv(basedir + 'clean_data_%s_epiweek-weekly_w_situation.csv' % pref,
                           encoding='utf-8', low_memory=False)
    df_cases['dado'] = pref
    df = mergedata_scale(df, df_cases)

    return df


def incidence_and_case_db(df, conn):

    cols = ['código', 'epiyear', 'epiweek', 'dado', 'escala', 'registros', 'data de execução']
    if len(set(cols) - set(df.columns)) == 0:
        df[cols].to_sql('serie_temporal', conn, if_exists='replace', index=False)
    else:
        print('Warning. Incidence not writen to DB. Columns don\'t match')

    return


def situation_db(df, conn):

    cols = ['código', 'epiyear', 'epiweek', 'situação', 'baixa', 'epidêmica', 'alta', 'muito alta', 'data de execução']
    if len(set(cols) - set(df.columns)) == 0:
        df[cols].to_sql('situacao', conn, if_exists='replace', index=False)
    else:
        print('Warning. Situation not writen to DB. Columns don\'t match')

    return


def estimates_db(df, conn):

    cols = ['código', 'base_epiyear', 'base_epiweek',
            'epiyear', 'epiweek', 'dado', 'escala', 'registros',
            'mediana', 'limite inferior', 'limite superior',
            'baixa', 'epidêmica', 'alta', 'muito alta',
            'data de execução']
    if len(set(cols) - set(df.columns)) == 0:
        df[cols].to_sql('estimativas', conn, if_exists='append', index=False)
    else:
        print('Warning. Estimates table not writen to DB. Columns don\'t match')

    return


def age_and_gender_db(df, conn):

    cols = ['código', 'epiyear', 'epiweek', 'dado', 'escala', 'sexo', '< 2 anos', '2-4 anos', '0-4 anos', '5-9 anos',
            '10-19 anos', '20-29 anos', '30-39 anos', '40-49 anos', '50-59 anos', '60+ anos']
    if len(set(cols) - set(df.columns)) == 0:
        df[cols].to_sql('faixa_etaria_e_genero', conn, if_exists='replace', index=False)
    else:
        print('Warning. Age bracket and gender table not writen to DB. Columns don\'t match')

    return


def report_db(df, conn):

    cols = ['código', 'dado', 'escala', 'limiar pré-epidêmico', 'intensidade alta', 'intensidade muito alta']
    if len(set(cols) - set(df.columns)) == 0:
        df[cols].to_sql('limiares', conn, if_exists='replace', index=False)
    else:
        print('Warning. Thresholds table not writen to DB. Columns don\'t match')

    return


def typical_db(df, conn):

    cols = ['código', 'epiweek', 'dado', 'escala', 'corredor baixo', 'corredor mediano', 'corredor alto']
    if len(set(cols) - set(df.columns)) == 0:
        df[cols].to_sql('corredores', conn, if_exists='replace', index=False)
    else:
        print('Warning. Typical activity table not writen to DB. Columns don\'t match')

    return


def main(update_db = False):
    dfpop = pd.read_csv('../data/PROJECOES_2013_POPULACAO-simples_v3_agebracket.csv', encoding='utf-8',
                        low_memory=False)
    dfpop.rename(columns={'UF': 'Unidade da Federação'}, inplace=True)
    dfpop.rename(columns={'Código': 'UF'}, inplace=True)

    if update_db:
        conn = sqlite3.connect('../../data/data/infogripe.db')

    preflist = ['srag', 'sragflu', 'obitoflu']

    for estimate_file in ['current_estimated', 'historical_estimated']:
        # Files current_estimated_values
        pref = preflist[0]
        df = pd.read_csv(basedir + '%s_%s_incidence.csv' % (pref, estimate_file), encoding='utf-8', low_memory=False)
        df['dado'] = pref
        df_new = convert_estimates(df, dfpop.loc[(dfpop.Sexo == 'Total'), ['UF', 'Ano', 'Total']])
        for pref in preflist[1:]:
            df = pd.read_csv(basedir + '%s_%s_incidence.csv' % (pref, estimate_file), encoding='utf-8',
                             low_memory=False)
            df['dado'] = pref
            df = convert_estimates(df, dfpop.loc[(dfpop.Sexo == 'Total'), ['UF', 'Ano', 'Total']])
            df_new = df_new.append(df, ignore_index=True)
        df_new.to_csv(basedir + '%s_values.csv' %estimate_file, index=False)

        df_new.rename(columns={
            'UF': 'código', 'SRAG': 'registros', '50%': 'mediana', '2.5%': 'limite inferior',
            '97.5%': 'limite superior', 'L0': 'baixa', 'L1': 'epidêmica', 'L2': 'alta',
            'L3': 'muito alta', 'Run date': 'data de execução', 'Situation': 'situação',
        }, inplace=True)
        if update_db:
            if estimate_file == 'historical_estimated':
                estimates_db(df_new, conn)
            else:
                incidence_and_case_db(df_new, conn)
                situation_db(df_new, conn)

    df_new = convert_report(preflist[0])
    for pref in preflist[1:]:
        df = convert_report(pref)
        df_new = df_new.append(df, ignore_index=True)
    df_new.to_csv(basedir + 'mem-report.csv', index=False)
    df_new.rename(columns={'UF': 'código'}, inplace=True)
    if update_db:
        report_db(df_new, conn)

    df_new = convert_typical(preflist[0])
    for pref in preflist[1:]:
        df = convert_typical(pref)
        df_new = df_new.append(df, ignore_index=True)
    df_new.to_csv(basedir + 'mem-typical.csv', index=False)
    df_new.rename(columns={'UF': 'código'}, inplace=True)
    if update_db:
        typical_db(df_new, conn)

    df_new = clean_data_merge(preflist[0])
    for pref in preflist[1:]:
        df = clean_data_merge(pref)
        df_new = df_new.append(df, ignore_index=True)
    df_new.to_csv(basedir + 'clean_data_epiweek-weekly-incidence_w_situation.csv', index=False)
    df_new.rename(columns={'UF': 'código'}, inplace=True)
    if update_db:
        age_and_gender_db(df_new, conn)
        conn.close()



if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Convert results from incidence to cases and merge datasets.\n" +
                                     "Exemple usage:\n" +
                                     "python3 consolidate_datasets.py",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--db', help='Update database or not.', default=False)
    args = parser.parse_args()
    main(args.db)
