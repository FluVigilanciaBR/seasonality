# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import pandas as pd
import argparse
from argparse import RawDescriptionHelpFormatter

def main(preflist):
    for pref in preflist:
        df_est = pd.read_csv('../clean_data/%s_current_estimated_values.csv' % pref, low_memory=False, encoding='utf-8')
        df_age = pd.read_csv('../clean_data/clean_data_%s_epiweek-weekly-incidence.csv' % pref, low_memory=False, encoding='utf-8')
        df_est_simp = df_est[['UF', 'epiyear', 'epiweek', 'Situation']]
        df_age_full = df_age.merge(df_est_simp, on=['UF', 'epiyear', 'epiweek'], how='left')
        df_age_full.dropna(axis=0, inplace=True)
        df_age_full.loc[df_age_full.Situation != 'stable', 'Situation'] = 'unknown'

        df_age_full.to_csv('../clean_data/clean_data_%s_epiweek-weekly-incidence_w_situation.csv' % pref, index=False,
                           encoding='utf-8')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Insert data status.\n" +
                                     "Exemple usage:\n" +
                                     "python3 sinan-convert2mem-fmt-regiao.py --path clean_data.csv",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--type', nargs='*', action='append', help='Prefix: srag, sragflu ou obitoflu',
                        default=['srag', 'sragflu', 'obitoflu'])
    args = parser.parse_args()
    main(args.type)
