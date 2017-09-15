import pandas as pd
from episem import episem, lastepiweek
import argparse
# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'


def main(fname, sep=','):

    df = pd.read_csv(fname, low_memory=False, encoding='utf-8')
    target_cols = ['DT_NOTIFIC', 'DT_DIGITA', 'DT_SIN_PRI']
    yearweek_cols = ['%s_epiyearweek' % k for k in target_cols]
    year_cols = ['%s_epiyear' % k for k in target_cols]
    week_cols = ['%s_epiweek' % k for k in target_cols]
    df[yearweek_cols] = df[target_cols].applymap(episem)
    df[year_cols] = df[target_cols].applymap(lambda x: episem(x, out='Y'))
    df[week_cols] = df[target_cols].applymap(lambda x: episem(x, out='W'))

    # Calculate opportunity between notification and upload:
    df['Notific2Digita_DelayWeeks'] = df['DT_DIGITA_epiweek'].astype(int) - df['DT_NOTIFIC_epiweek'].astype(int) +\
        (df['DT_DIGITA_epiyear'].astype(int) - df['DT_NOTIFIC_epiyear'].astype(int)) *\
        (df['DT_NOTIFIC_epiyear'].apply(lastepiweek)).astype(int)

    df['SinPri2Digita_DelayWeeks'] = df['DT_DIGITA_epiweek'].astype(int) - df['DT_SIN_PRI_epiweek'].astype(int) +\
        (df['DT_DIGITA_epiyear'].astype(int) - df['DT_SIN_PRI_epiyear'].astype(int)) *\
        (df['DT_SIN_PRI_epiyear'].apply(lastepiweek)).astype(int)


    fout = '../clean_data/%s_epiweek.csv' % fname.split('/')[-1][:-4]
    df.to_csv(fout, index=False, encoding='utf-8')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Insert Brazilian epiweek columns")
    parser.add_argument('--path', help='Path to data file')
    parser.add_argument('--sep', help='Column separator', default=',')
    args = parser.parse_args()
    main(fname=args.path, sep=args.sep)
