import pandas as pd
from episem import episem
import argparse
# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'


def main(fname, sep=','):

    df = pd.read_csv(fname, low_memory=False)
    target_cols = ['DT_NOTIFIC', 'DT_DIGITA']
    yearweek_cols = ['%s_epiyearweek' % k for k in target_cols]
    year_cols = ['%s_epiyear' % k for k in target_cols]
    week_cols = ['%s_epiweek' % k for k in target_cols]
    df[yearweek_cols] = df[target_cols].applymap(episem)
    df[year_cols] = df[target_cols].applymap(lambda x: episem(x, out='Y'))
    df[week_cols] = df[target_cols].applymap(lambda x: episem(x, out='W'))

    fout = '../clean_data/%s_epiweek.csv' % fname.split('/')[-1][:-4]
    df.to_csv(fout, index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Insert Brazilian epiweek columns")
    parser.add_argument('--path', help='Path to data file')
    parser.add_argument('--sep', help='Column separator', default=',')
    args = parser.parse_args()
    main(fname=args.path, sep=args.sep)
