# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import pandas as pd
import argparse
from argparse import RawDescriptionHelpFormatter


def convert_estimates(df, dfpop):
    # Files current_estimated_values
    tgt_cols = ['SRAG', 'mean', '50%', '2.5%',  '97.5%']
    for uf in df.UF.unique():
        df_slice = df[df.UF == uf].copy()
        dfpop_slice = dfpop[(dfpop.UF == uf)]
        for year in df_slice.epiyear.unique():
            df_slice.loc[(df_slice.epiyear == year), tgt_cols] *= dfpop_slice.loc[dfpop_slice.Ano == year,
                                                                                  'Total'].values[0]/100000

        df[df.UF == uf] = df_slice

    return df


def convert_clean_data(df, dfpop):
    # TODO: for the cases, age brackets should start at <2 yo, 2-4, and then as it is for incidence.
    # Files clean_data_*epiweek-weekly-incidence_w_situation
    age_cols = ['Total', '0-4 anos', '5-9 anos', '10-19 anos', '20-29 anos', '30-39 anos', '40-49 anos',
                '50-59 anos', '60+ anos']
    df.rename(columns={'SRAG': 'Total'}, inplace=True)

    for uf in df.UF.unique():
        df_slice = df[df.UF == uf].copy()
        for year in df_slice.epiyear.unique():
            for sex in ['Total', 'M', 'F']:
                dfpop_slice = dfpop[(dfpop.Sexo == sex)]
                dfpop_slice.set_index('Ano', inplace=True)
                tgt_rows = (df_slice.UF == uf) & (df_slice.epiyear == year) & (df_slice.sexo == sex)
                dfpop_tgt_rows = (dfpop_slice.UF == str(uf)) & (dfpop_slice.Sexo == sex) & (dfpop_slice.index == year)
                df_slice.loc[tgt_rows, age_cols] = df_slice.loc[tgt_rows, age_cols]. \
                    multiply(dfpop_slice.loc[dfpop_tgt_rows, age_cols].ix[year], axis='columns')/100000

        df[df.UF == uf] = df_slice
    df.rename(columns={'Tota': 'SRAG'}, inplace = True)

    return df


def convert_report(df):
    # Files mem-report
    tgt_cols = ['Média geométrica do pico de infecção das temporadas regulares',
                'limiar pré-epidêmico',
                'intensidade alta',
                'intensidade muito alta']
    for col in tgt_cols:
        df[col] *= df['População']/100000

    return df


def convert_typical(df):
    # Files mem-typical
    tgt_cols = ['corredor baixo', 'corredor mediano', 'corredor alto']
    for col in tgt_cols:
        df[col] *= df['População']/100000

    return df


def main(preflist):
    basedir = '../clean_data/'
    dfpop = pd.read_csv('../data/PROJECOES_2013_POPULACAO-simples_v3_agebracket.csv', encoding='utf-8')
    dfpop.rename(columns={'UF': 'Unidade da Federação'}, inplace=True)
    dfpop.rename(columns={'Código': 'UF'}, inplace=True)

    for pref in preflist:
        df = pd.read_csv(basedir + '%s_current_estimated_values.csv' % pref, encoding='utf-8')
        df = convert_estimates(df, dfpop.loc[(dfpop.Sexo == 'Total'), ['UF', 'Ano', 'Total']])
        df.to_csv(basedir + '%s_current_estimated_values_cases.csv' % pref, index=False)

        df = pd.read_csv(basedir + 'clean_data_%s_epiweek-weekly-incidence_w_situation.csv' % pref, encoding='utf-8')
        df = convert_clean_data(df, dfpop)
        df.to_csv(basedir + 'clean_data_%s_epiweek-weekly-incidence_w_situation_cases.csv' % pref, index=False)

        df = pd.read_csv(basedir + '%s_mem-report.csv' % pref, encoding='utf-8')
        df = convert_report(df)
        df.to_csv(basedir + '%s_mem-report_cases.csv' % pref, index=False)

        df = pd.read_csv(basedir + '%s_mem-typical.csv' % pref, encoding='utf-8')
        df = convert_typical(df)
        df.to_csv(basedir + '%s_mem-typical_cases.csv' % pref, index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Convert results from incidence to cases.\n" +
                                     "Exemple usage:\n" +
                                     "python3 sinan-convert2mem-fmt-regiao.py --path clean_data.csv",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--type', nargs='*', action='append', help='Prefix: srag, sragflu ou obitoflu',
                        default=['srag', 'sragflu', 'obitoflu'])
    args = parser.parse_args()
    main(args.type)
