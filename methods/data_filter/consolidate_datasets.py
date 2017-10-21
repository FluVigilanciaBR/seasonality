# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

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


def main():
    dfpop = pd.read_csv('../data/PROJECOES_2013_POPULACAO-simples_v3_agebracket.csv', encoding='utf-8',
                        low_memory=False)
    dfpop.rename(columns={'UF': 'Unidade da Federação'}, inplace=True)
    dfpop.rename(columns={'Código': 'UF'}, inplace=True)

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

    df_new = convert_report(preflist[0])
    for pref in preflist[1:]:
        df = convert_report(pref)
        df_new = df_new.append(df, ignore_index=True)
    df_new.to_csv(basedir + 'mem-report.csv', index=False)

    df_new = convert_typical(preflist[0])
    for pref in preflist[1:]:
        df = convert_typical(pref)
        df_new = df_new.append(df, ignore_index=True)
    df_new.to_csv(basedir + 'mem-typical.csv', index=False)

    df_new = clean_data_merge(preflist[0])
    for pref in preflist[1:]:
        df = clean_data_merge(pref)
        df_new = df_new.append(df, ignore_index=True)
    df_new.to_csv(basedir + 'clean_data_epiweek-weekly-incidence_w_situation.csv', index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Convert results from incidence to cases and merge datasets.\n" +
                                     "Exemple usage:\n" +
                                     "python3 consolidate_datasets.py",
                                     formatter_class=RawDescriptionHelpFormatter)
    main()
