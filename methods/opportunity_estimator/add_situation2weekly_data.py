# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import pandas as pd
import argparse
from argparse import RawDescriptionHelpFormatter


def main(preflist, filtertype='srag'):

    if filtertype not in ['srag', 'sragnofever', 'hospdeath']:
        exit('Invalid filter type: %s' % filtertype)

    def mergesituation(dfa, dfb):
        dfa = dfa.merge(dfb, on=['UF', 'epiyear', 'epiweek'], how='left')
        dfa.dropna(axis=0, inplace=True)
        dfa.loc[dfa.Situation != 'stable', 'Situation'] = 'unknown'
        return dfa

    suff = ''
    if filtertype != 'srag':
        suff = '_%s' % filtertype

    for pref in preflist:
        df_est = pd.read_csv('../clean_data/%s%s_current_estimated_incidence.csv' % (pref, suff), low_memory=False,
                             encoding='utf-8')
        df_age = pd.read_csv('../clean_data/clean_data_%s%s_epiweek-weekly-incidence.csv' % (pref, suff),
                             low_memory=False, encoding='utf-8')
        df_age_cases = pd.read_csv('../clean_data/clean_data_%s%s_epiweek-weekly.csv' % (pref, suff), low_memory=False,
                                   encoding='utf-8')
        df_est_simp = df_est[['UF', 'epiyear', 'epiweek', 'Situation']]
        df_age = mergesituation(df_age, df_est_simp)
        df_age_cases = mergesituation(df_age_cases, df_est_simp)

        df_age.to_csv('../clean_data/clean_data_%s%s_epiweek-weekly-incidence_w_situation.csv' % (pref, suff),
                      index=False,
                      encoding='utf-8')
        df_age_cases.to_csv('../clean_data/clean_data_%s%s_epiweek-weekly_w_situation.csv' % (pref, suff),
                            index=False,
                            encoding='utf-8')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Insert data status.\n" +
                                     "Example usage:\n" +
                                     "python3 add_situation2weekly_data.py --type srag sragflu obitoflu",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--type', nargs='*', action='append', help='Prefix: srag, sragflu ou obitoflu',
                        default=['srag', 'sragflu', 'obitoflu'])
    parser.add_argument('--filter', help='Filter type: srag, nofever, hospdeath',
                        default='srag')
    args = parser.parse_args()
    main(args.type, args.filter)
