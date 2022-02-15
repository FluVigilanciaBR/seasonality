# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import argparse
from argparse import RawDescriptionHelpFormatter

import pandas as pd

from .contingency_level import weekly_alert_table_all, calc_season_alert, calc_season_contingency, \
    get_all_territories_and_years
from .migration import migrate_from_csv_to_psql

basedir = '../clean_data/'
outdir = '../../data/data/'
upper_bounds = [8, 10, 12, 14, 16, 18]


def mergedata_scale(df, df_cases):
    df['escala'] = 'incidência'
    df_cases['escala'] = 'casos'

    df = pd.concat([df, df_cases], ignore_index=True, sort=True)

    return df


def convert_estimates(df, dfpop):

    tgt_cols = ['SRAG', 'mean', '50%', '2.5%', '5%', '25%', '75%', '95%', '97.5%']
    df[tgt_cols] = df[tgt_cols].transform({col: lambda x: min(x, 100000) for col in tgt_cols})
    df = df.merge(dfpop, how='left', left_on=['UF', 'epiyear'], right_on=['UF', 'Ano']).rename(columns={
        'Total': 'population'
    }).drop(columns='Ano')

    df_cases = df.copy()
    df_cases[tgt_cols] = df_cases[tgt_cols].multiply(df_cases.population / 100000, axis='index')
    df_cases[tgt_cols] = df_cases[tgt_cols].applymap(lambda x: round(x, 0))

    # Apply upper bound to CI:
    df_cases['bounded_97.5%'] = df_cases[['50%', '97.5%']].apply(lambda x: min(x['97.5%'], 3 * x['50%']), axis=1)
    tgt_rows = (df_cases['50%'] < 6.0)
    df_cases.loc[tgt_rows, 'bounded_97.5%'] = df_cases.loc[tgt_rows, ['50%', '97.5%']].apply(lambda x:
                                                                                             min(
                                                                                                 x['97.5%'],
                                                                                                 upper_bounds[
                                                                                                     int(x['50%'])
                                                                                                 ]
                                                                                                 ), axis=1)

    col_match = ['epiyear', 'epiweek']
    if 'base_epiweek' in df.columns:
        col_match.extend(['base_epiyear', 'base_epiweek'])

    df_cases['cntry_percentage'] = df_cases[col_match + ['50%']].merge(df_cases.loc[df_cases.UF == 'BR',
                                                                                    col_match + ['50%']],
                                                                       on=col_match,
                                                                       how='left')['50%_y']
    df_cases.cntry_percentage = 100 * df_cases['50%'] / df_cases.cntry_percentage
    df[['bounded_97.5%', 'cntry_percentage']] = df[['UF'] + col_match].merge(df_cases[['UF'] + col_match +
                                                                                      ['bounded_97.5%',
                                                                                       'cntry_percentage']],
                                                                             on=['UF'] + col_match,
                                                                             how='left')[['bounded_97.5%',
                                                                                          'cntry_percentage']]
    df['bounded_97.5%'] *= 100000 / df.population
    df = mergedata_scale(df, df_cases)
    return df


def convert_report(pref, suff):
    # Files mem-report
    df = pd.read_csv(basedir + '%s_mem-report.csv' % (pref + suff), encoding='utf-8', low_memory=False)
    df['dado'] = pref
    df_cases = df.copy()
    tgt_cols = ['Média geométrica do pico de infecção das temporadas regulares',
                'limiar pré-epidêmico',
                'intensidade alta',
                'intensidade muito alta']
    for col in tgt_cols:
        df_cases[col] *= df_cases['População'] / 100000

    df = mergedata_scale(df, df_cases)
    return df


def convert_typical(pref, suff):
    # Files mem-typical
    df = pd.read_csv(basedir + '%s_mem-typical.csv' % (pref + suff), encoding='utf-8', low_memory=False)
    df['dado'] = pref
    df_cases = df.copy()
    tgt_cols = ['corredor baixo', 'corredor mediano', 'corredor alto']
    for col in tgt_cols:
        df_cases[col] *= df_cases['População'] / 100000

    df = mergedata_scale(df, df_cases)
    return df


def clean_data_merge(pref, suff):
    df = pd.read_csv(basedir + 'clean_data_%s_epiweek-weekly-incidence_w_situation.csv.gz' % (pref + suff),
                     encoding='utf-8',
                     low_memory=False)
    df['dado'] = pref
    df_cases = pd.read_csv(basedir + 'clean_data_%s_epiweek-weekly_w_situation.csv.gz' % (pref + suff),
                           encoding='utf-8', low_memory=False)
    df_cases['dado'] = pref
    df = mergedata_scale(df, df_cases)

    return df


def rolling_average(df, win: int=3):
    df = df.sort_values(by=['dado', 'escala', 'UF', 'epiyear', 'epiweek']).reset_index(drop=True)
    df['rolling_average'] = df.groupby(by=['dado', 'escala', 'UF'])['50%'].rolling(win, center=True).mean().reset_index(
        drop=True)

    mask = (df.escala == 'casos')
    df.loc[mask, 'rolling_average'] = df.loc[mask, 'rolling_average'].round(0)

    return df


def main(update_db=False, filtertype='srag'):
    if filtertype not in ['srag', 'sragnofever', 'hospdeath']:
        exit('Invalid filter type: %s' % filtertype)

    suff = ''
    if filtertype != 'srag':
        suff = '_%s' % filtertype

    dfpop = pd.read_csv('../data/PROJECOES_2013_POPULACAO-simples_v3_agebracket.csv', encoding='utf-8',
                        low_memory=False)
    dfpop.rename(columns={'UF': 'Unidade da Federação'}, inplace=True)
    dfpop.rename(columns={'Código': 'UF'}, inplace=True)
    dfpop_tot = dfpop.loc[(dfpop.Sexo == 'Total'), ['UF', 'Ano', 'Total']]

    if update_db:
        dfdict = {}

    preflist = ['srag', 'sragflu', 'obitoflu', 'sragcovid', 'obitocovid', 'obito']

    for estimate_file in ['current_estimated', 'historical_estimated']:
        # Files current_estimated_values
        pref = preflist[0]
        df = pd.read_csv(basedir + '%s%s_%s_incidence.csv.gz' % (pref, suff, estimate_file), encoding='utf-8',
                         low_memory=False)
        df['dado'] = pref
        df_new = convert_estimates(df, dfpop.loc[(dfpop.Sexo == 'Total'), ['UF', 'Ano', 'Total']])
        for pref in preflist[1:]:
            df = pd.read_csv(basedir + '%s%s_%s_incidence.csv.gz' % (pref, suff, estimate_file), encoding='utf-8',
                             low_memory=False)
            df['dado'] = pref
            df = convert_estimates(df, dfpop_tot)
            df_new = pd.concat([df_new, df], ignore_index=True, sort=True)

        if estimate_file == 'current_estimated':
            df_new = rolling_average(df_new)

        fname = '%s_values' % estimate_file + suff
        df_new.to_csv(outdir + fname + '.csv.gz', index=False)
        if update_db:
            dfdict[fname] = df_new

    df_new = convert_report(preflist[0], suff)
    for pref in preflist[1:]:
        df = convert_report(pref, suff)
        df_new = pd.concat([df_new, df], ignore_index=True, sort=True)
    fname = 'mem-report' + suff
    df_new.to_csv(outdir + fname + '.csv', index=False)
    if update_db:
        dfdict[fname] = df_new

    df_new = convert_typical(preflist[0], suff)
    for pref in preflist[1:]:
        df = convert_typical(pref, suff)
        df_new = pd.concat([df_new, df], ignore_index=True, sort=True)
    fname = 'mem-typical' + suff
    df_new.to_csv(outdir + fname + '.csv', index=False)
    if update_db:
        dfdict[fname] = df_new

    df_new = clean_data_merge(preflist[0], suff)
    for pref in preflist[1:]:
        df = clean_data_merge(pref, suff)
        df_new = pd.concat([df_new, df], ignore_index=True, sort=True)
    fname = 'clean_data_epiweek-weekly-incidence_w_situation' + suff
    df_new.to_csv(outdir + fname + '.csv.gz', index=False)
    if update_db:
        dfdict[fname] = df_new
        migrate_from_csv_to_psql(dfs=dfdict, suff=suff)
        dfdict = {}

    fname = 'contingency_level' + suff
    df = get_all_territories_and_years(filtertype=filtertype)
    df_new = calc_season_contingency(filtertype=filtertype)
    df_new.to_csv(outdir + fname + '.csv', index=False)
    if update_db:
        dfdict[fname] = df_new

    fname = 'weekly_alert' + suff
    df_new = weekly_alert_table_all(df, filtertype=filtertype)
    df_new.to_csv(outdir + fname + '.csv', index=False)
    if update_db:
        dfdict[fname] = df_new

    fname = 'season_level' + suff
    df_new = calc_season_alert(dfdict['weekly_alert' + suff])
    df_new.to_csv(outdir + fname + '.csv', index=False)
    if update_db:
        dfdict[fname] = df_new
        fname = 'delay_table' + suff
        dfdict[fname] = pd.read_csv(outdir + fname + '.csv.gz')
        migrate_from_csv_to_psql(dfs=dfdict, basic_tables=False, suff=suff)

    return


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Convert results from incidence to cases and merge datasets.\n" +
                                                 "Exemple usage:\n" +
                                                 "python3 consolidate_datasets.py",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--db', help='Update database', action='store_true')
    args = parser.parse_args()
    main(args.db)
