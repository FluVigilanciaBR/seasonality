import pandas as pd
from .episem import episem, lastepiweek
import argparse
# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'



def opportunity_calc_epiweeks(df, colA, colB, colnew):
    print(colA, colB)
    mask = (df.DT_NOTIFIC_epiyear != 2013)
    mask = (mask & pd.notnull(df[[colA, colB]]).all(axis=1))
    print(sum(mask))
    df.loc[mask, colnew] = df.loc[mask, '%s_epiweek' % colB].astype(int) - df.loc[mask, '%s_epiweek' % colA].astype(
        int) + (df.loc[mask, '%s_epiyear' % colB].astype(int) - df.loc[mask, '%s_epiyear' % colA].astype(int)) * \
                                  (df.loc[mask, '%s_epiyear' % colA].apply(lastepiweek)).astype(int)
    df.loc[df[colnew] < 0, colnew] = None
    return(df)


def opportunity_calc_days(df, colA, colB, colnew):
    print(colA, colB)
    mask = (df.DT_NOTIFIC_epiyear != 2013)
    mask = (mask & pd.notnull(df[[colA, colB]]).all(axis=1))
    print(sum(mask))
    df.loc[mask, colnew] = (pd.to_datetime(df.loc[mask, colB]) - pd.to_datetime(df.loc[mask, colA])).dt.days
    df.loc[df[colnew] < 0, colnew] = None

    return(df)


def main(fname, sep=','):

    df = pd.read_csv(fname, low_memory=False, encoding='utf-8')
    target_cols = ['DT_NOTIFIC', 'DT_DIGITA', 'DT_SIN_PRI', 'DT_ANTIVIR', 'DT_COLETA', 'DT_IFI', 'DT_PCR_1',
                   'DT_ENCERRA']
    yearweek_cols = ['%s_epiyearweek' % k for k in target_cols]
    year_cols = ['%s_epiyear' % k for k in target_cols]
    week_cols = ['%s_epiweek' % k for k in target_cols]
    df[yearweek_cols] = df[target_cols].applymap(episem)
    df[year_cols] = df[target_cols].applymap(lambda x: episem(x, out='Y'))
    df[week_cols] = df[target_cols].applymap(lambda x: episem(x, out='W'))

    # Calculate opportunities:
    df = opportunity_calc_epiweeks(df, colA='DT_SIN_PRI', colB='DT_DIGITA', colnew='SinPri2Digita_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_SIN_PRI', colB='DT_ANTIVIR', colnew='SinPri2Antivir_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_SIN_PRI', colB='DT_NOTIFIC', colnew='SinPri2Notific_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_SIN_PRI', colB='DT_COLETA', colnew='SinPri2Coleta_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_NOTIFIC', colB='DT_ENCERRA', colnew='Notific2Encerra_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_COLETA', colB='DT_IFI', colnew='Coleta2IFI_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_COLETA', colB='DT_PCR_1', colnew='Coleta2PCR_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_NOTIFIC', colB='DT_DIGITA', colnew='Notific2Digita_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_NOTIFIC', colB='DT_COLETA', colnew='Notific2Coleta_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_NOTIFIC', colB='DT_ANTIVIR', colnew='Notific2Antivir_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_DIGITA', colB='DT_ANTIVIR', colnew='Digita2Antivir_DelayWeeks')

    df = opportunity_calc_days(df, colA='DT_SIN_PRI', colB='DT_DIGITA', colnew='SinPri2Digita_DelayDays')
    df = opportunity_calc_days(df, colA='DT_SIN_PRI', colB='DT_ANTIVIR', colnew='SinPri2Antivir_DelayDays')
    df = opportunity_calc_days(df, colA='DT_SIN_PRI', colB='DT_NOTIFIC', colnew='SinPri2Notific_DelayDays')
    df = opportunity_calc_days(df, colA='DT_SIN_PRI', colB='DT_COLETA', colnew='SinPri2Coleta_DelayDays')
    df = opportunity_calc_days(df, colA='DT_NOTIFIC', colB='DT_ENCERRA', colnew='Notific2Encerra_DelayDays')
    df = opportunity_calc_days(df, colA='DT_COLETA', colB='DT_IFI', colnew='Coleta2IFI_DelayDays')
    df = opportunity_calc_days(df, colA='DT_COLETA', colB='DT_PCR_1', colnew='Coleta2PCR_DelayDays')
    df = opportunity_calc_days(df, colA='DT_NOTIFIC', colB='DT_DIGITA', colnew='Notific2Digita_DelayDays')
    df = opportunity_calc_days(df, colA='DT_NOTIFIC', colB='DT_COLETA', colnew='Notific2Coleta_DelayDays')
    df = opportunity_calc_days(df, colA='DT_NOTIFIC', colB='DT_ANTIVIR', colnew='Notific2Antivir_DelayDays')
    df = opportunity_calc_days(df, colA='DT_DIGITA', colB='DT_ANTIVIR', colnew='Digita2Antivir_DelayDays')

    fout = '../clean_data/%s_epiweek.csv' % fname.split('/')[-1][:-4]
    df.to_csv(fout, index=False, encoding='utf-8')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Insert Brazilian epiweek columns")
    parser.add_argument('--path', help='Path to data file')
    parser.add_argument('--sep', help='Column separator', default=',')
    args = parser.parse_args()
    main(fname=args.path, sep=args.sep)
