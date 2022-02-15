# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import datetime

import epiweeks
import pandas as pd
import argparse
import logging

module_logger = logging.getLogger('update_system.insert_epiweek')


def opportunity_calc_epiweeks(df, colA, colB, colnew):
    mask = (pd.notnull(df[[colA, colB]]).all(axis=1))
    # df.loc[mask, colnew] = df.loc[mask, '%s_epiweek' % colB].astype(int) - df.loc[mask, '%s_epiweek' % colA].astype(
    #     int) + (df.loc[mask, '%s_epiyear' % colB].astype(int) - df.loc[mask, '%s_epiyear' % colA].astype(int)) * \
    #                        (df.loc[mask, '%s_epiyear' % colA].apply(lastepiweek)).astype(int)
    df.loc[mask, colnew] = (df.loc[mask, colB].apply(lambda x: epiweeks.Week.fromdate(x).enddate()) -
                            df.loc[mask, colA].apply(lambda x: epiweeks.Week.fromdate(x).enddate())).dt.days/7
    df.loc[df[colnew] < 0, colnew] = None
    return df


def opportunity_calc_days(df, colA, colB, colnew):
    mask = (pd.notnull(df[[colA, colB]]).all(axis=1))
    df.loc[mask, colnew] = (pd.to_datetime(df.loc[mask, colB]) - pd.to_datetime(df.loc[mask, colA])).dt.days
    df.loc[df[colnew] < 0, colnew] = None

    return df


def date2epiweek(x, out='YW'):
    if pd.isnull(x):
        return None
    if out == 'YW':
        return epiweeks.Week.fromdate(x).isoformat()
    if out == 'Y':
        return epiweeks.Week.fromdate(x).year
    if out == 'W':
        return '%02d' % epiweeks.Week.fromdate(x).week


def insert_epiweek(df):
    module_logger.info('Entering function: inser_epiweek')
    target_cols = ['DT_NOTIFIC', 'DT_DIGITA', 'DT_SIN_PRI', 'DT_ANTIVIR', 'DT_COLETA', 'DT_IFI', 'DT_PCR',
                   'DT_ENCERRA', 'DT_INTERNA', 'DT_EVOLUCA']
    df_slice = df[target_cols].apply(pd.to_datetime).copy()
    yearweek_cols = ['%s_epiyearweek' % k for k in target_cols]
    year_cols = ['%s_epiyear' % k for k in target_cols]
    week_cols = ['%s_epiweek' % k for k in target_cols]
    df[yearweek_cols] = df_slice.applymap(lambda x: date2epiweek(x))
    df[year_cols] = df_slice.applymap(lambda x: date2epiweek(x, out='Y'))
    df[week_cols] = df_slice.applymap(lambda x: date2epiweek(x, out='W'))

    # Calculate opportunities:
    df = opportunity_calc_epiweeks(df, colA='DT_SIN_PRI', colB='DT_DIGITA', colnew='SinPri2Digita_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_SIN_PRI', colB='DT_ANTIVIR', colnew='SinPri2Antivir_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_SIN_PRI', colB='DT_NOTIFIC', colnew='SinPri2Notific_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_SIN_PRI', colB='DT_COLETA', colnew='SinPri2Coleta_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_SIN_PRI', colB='DT_INTERNA', colnew='SinPri2Interna_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_NOTIFIC', colB='DT_ENCERRA', colnew='Notific2Encerra_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_COLETA', colB='DT_IFI', colnew='Coleta2IFI_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_COLETA', colB='DT_PCR', colnew='Coleta2PCR_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_NOTIFIC', colB='DT_DIGITA', colnew='Notific2Digita_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_NOTIFIC', colB='DT_COLETA', colnew='Notific2Coleta_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_NOTIFIC', colB='DT_ANTIVIR', colnew='Notific2Antivir_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_DIGITA', colB='DT_ANTIVIR', colnew='Digita2Antivir_DelayWeeks')
    df = opportunity_calc_epiweeks(df, colA='DT_INTERNA', colB='DT_EVOLUCA', colnew='Interna2Evoluca_DelayWeeks')

    df = opportunity_calc_days(df, colA='DT_SIN_PRI', colB='DT_DIGITA', colnew='SinPri2Digita_DelayDays')
    df = opportunity_calc_days(df, colA='DT_SIN_PRI', colB='DT_ANTIVIR', colnew='SinPri2Antivir_DelayDays')
    df = opportunity_calc_days(df, colA='DT_SIN_PRI', colB='DT_NOTIFIC', colnew='SinPri2Notific_DelayDays')
    df = opportunity_calc_days(df, colA='DT_SIN_PRI', colB='DT_COLETA', colnew='SinPri2Coleta_DelayDays')
    df = opportunity_calc_days(df, colA='DT_SIN_PRI', colB='DT_INTERNA', colnew='SinPri2Interna_DelayDays')
    df = opportunity_calc_days(df, colA='DT_NOTIFIC', colB='DT_ENCERRA', colnew='Notific2Encerra_DelayDays')
    df = opportunity_calc_days(df, colA='DT_COLETA', colB='DT_IFI', colnew='Coleta2IFI_DelayDays')
    df = opportunity_calc_days(df, colA='DT_COLETA', colB='DT_PCR', colnew='Coleta2PCR_DelayDays')
    df = opportunity_calc_days(df, colA='DT_NOTIFIC', colB='DT_DIGITA', colnew='Notific2Digita_DelayDays')
    df = opportunity_calc_days(df, colA='DT_NOTIFIC', colB='DT_COLETA', colnew='Notific2Coleta_DelayDays')
    df = opportunity_calc_days(df, colA='DT_NOTIFIC', colB='DT_ANTIVIR', colnew='Notific2Antivir_DelayDays')
    df = opportunity_calc_days(df, colA='DT_DIGITA', colB='DT_ANTIVIR', colnew='Digita2Antivir_DelayDays')
    df = opportunity_calc_days(df, colA='DT_INTERNA', colB='DT_EVOLUCA', colnew='Interna2Evoluca_DelayDays')

    return df


def main(fname, sep=','):

    df = pd.read_csv(fname, low_memory=False, encoding='utf-8')
    df = insert_epiweek(df)
    fout = '../clean_data/%s_epiweek.csv' % fname.split('/')[-1].rstrip('.csv')
    df.to_csv(fout, index=False, encoding='utf-8')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Insert Brazilian epiweek columns")
    parser.add_argument('--path', help='Path to data file')
    parser.add_argument('--sep', help='Column separator', default=',')
    args = parser.parse_args()
    main(fname=args.path, sep=args.sep)
