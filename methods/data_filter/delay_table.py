__author__ = 'Marcelo Ferreira da Costa Gomes'

import sys
import logging
import pandas as pd
import numpy as np
import random
from .episem import lastepiweek, epiweek2date

module_logger = logging.getLogger('update_system.delay_table')


def extract_quantile(dforig=pd.DataFrame, filtertype='srag'):
    if filtertype not in ['srag', 'sragnofever', 'hospdeath']:
        exit('Invalid filter type: %s' % filtertype)

    suff = ''
    if filtertype != 'srag':
        suff = '_%s' % filtertype

    dfquant = pd.DataFrame(columns=['UF', 'dado', 'epiyearweek', 'epiyear', 'epiweek'])
    df = dforig[(dforig.delayweeks <= 26) & (dforig.epiyear >= 2013)].copy()
    tgt_cols = ['UF', 'Regional', 'Regiao', 'Pais']
    uflist={col: dforig[col].unique().tolist() for col in tgt_cols}
    for year in range(2014, df.epiyear.max()+1):
        if year == df.epiyear.max():
            weekmax = int(max(df.DT_DIGITA_epiweek[df.DT_DIGITA_epiyear == year]) + 1)
        else:
            weekmax = int(lastepiweek(year)) + 1
        for week in range(1, weekmax):
            f_epiweek = '%sW%02d' % (year-2, week)
            l_epiweek = '%sW%02d' % (year, week)
            dftmp = df.loc[(df.epiyearweek >= f_epiweek) & (df.DT_DIGITA_epiyearweek <= l_epiweek),
                           tgt_cols + ['dado', 'delayweeks']]
            for tgt_col in tgt_cols:
                out_cols = [tgt_col, 'dado', 'delayweeks']
                dfqtmp = pd.DataFrame([{tgt_col: uf, 'dado': dado, 'epiyearweek': '%sW%02d' % (year, week), 'epiyear':
                    year, 'epiweek': week} for uf in uflist[tgt_col] for dado in ['srag', 'sragflu', 'obitoflu',
                                                                                  'sragcovid', 'obitocovid', 'obito']])

                dfqtmp = dfqtmp.merge((np.ceil(dftmp[out_cols].groupby(out_cols[0:2]).quantile(.95))).reset_index()[
                    out_cols], on=[tgt_col, 'dado'], how='left').fillna(0)
                dfquant = dfquant.append(dfqtmp.rename(columns={tgt_col: 'UF'}), ignore_index=True, sort=False)

    dfquant.sort_values(by=['dado', 'UF', 'epiyearweek'], inplace=True)
    dfquant.to_csv('../clean_data/sinpri2digita_quantiles%s.csv' % suff, index=False)

    return


def readtable(fname):
    """Read notification table.

    :param fname: path to file
    :return: pandas.DataFrame
    """
    df = pd.read_csv(fname, low_memory=False)[
        ['UF', 'Regional', 'Regiao', 'Pais', 'dado', 'DT_SIN_PRI_epiyearweek', 'DT_SIN_PRI_epiyear',
         'DT_SIN_PRI_epiweek', 'SinPri2Digita_DelayWeeks', 'DT_DIGITA_epiyearweek', 'DT_DIGITA_epiyear',
         'DT_DIGITA_epiweek']
    ].rename(columns={'DT_SIN_PRI_epiyearweek': 'epiyearweek', 'DT_SIN_PRI_epiyear': 'epiyear',
                      'DT_SIN_PRI_epiweek': 'epiweek', 'SinPri2Digita_DelayWeeks': 'delayweeks'})
    df.UF = df.UF.astype(int).astype(str)

    # dfmunreg = pd.read_csv('~/dropbox-personal/Dropbox/IBGE/rl_municip_regsaud.csv', sep=';', header=None,
    #                        names=['municipio', 'regionalsaude'])

    return df


def delayimputation(df=pd.DataFrame()):
    """
    Generate delay estimate for missing data, from locality and epiweek profile

    :param df: original data frame
    :return: df: original data with imputed data
    """

    # Count delay frequency by locality and 4-epiweek periods since 2013:
    dfgrp = df[df.epiyear >= 2013].copy()
    dfgrp['4weeks'] = dfgrp.epiweek.map(lambda x: 1 + min(12, int((x-1)/4)))
    dfgrp = dfgrp.groupby(['UF', '4weeks', 'delayweeks']).size().reset_index()
    dfgrp.rename(columns={0: 'counts'}, inplace=True)
    dfgrp.delayweeks = dfgrp.delayweeks.astype(int)
    dfgrp_uf = dfgrp[['UF', 'delayweeks', 'counts']].groupby(['UF', 'delayweeks'], as_index=False).agg(sum)
    today_year = int(df.DT_DIGITA_epiyear.max())
    today_week = int(df.DT_DIGITA_epiweek[df.DT_DIGITA_epiyear == today_year].max())

    df['dlimit'] = today_week - df.epiweek.astype(int) + (today_year - df.epiyear) * \
                   (df.epiyear.apply(lastepiweek)).astype(int)

    def sampler(col):
        dfgrp_slice = dfgrp.loc[(dfgrp['UF'] == col['UF']) &
                                (dfgrp['4weeks'] == col['4weeks']) &
                                (dfgrp['delayweeks'] <= col['dlimit']),
                                ['delayweeks', 'counts']]
        if len(dfgrp_slice) == 0:
            dfgrp_slice = dfgrp_uf.loc[(dfgrp_uf['UF'] == col['UF']) & (dfgrp_uf['delayweeks'] <= col['dlimit']),
                                       ['delayweeks', 'counts']]

        d = random.choices(population=dfgrp_slice['delayweeks'].values, weights=dfgrp_slice['counts'].values)[0]
        return d

    df['4weeks'] = df.epiweek.map(lambda x: 1 + min(12, int((x-1)/4)))
    df.loc[pd.isnull(df.delayweeks), 'delayweeks'] = df.loc[pd.isnull(df.delayweeks), ['UF', '4weeks', 'dlimit']].apply(
        sampler, axis=1)

    df.drop(columns=['4weeks', 'dlimit'], inplace=True)

    return df


def createtable(df_in = pd.DataFrame()):
    """Generate digitalization opportunity table by epidemiological week.

    From notification data, creates a table with total number of notified cases on column Notifications
    and number of cases introduced in the database by number of weeks since notification.

    The output is a pandas.DataFrame with columns:

    locality | epiyear | epiweek | Notifications | d0 | d1 | ... | d27 | Notifications_26w

    Where:

     * locality: has the Federation Unit code;
     * epiyear: epidemiological year;
     * epiweek: epidemiological week;
     * Notifications: total number of notified cases for that locality, epiyear, epiweek so far;
     * dn: corresponds to the number of cases digitalized n weeks *after* notification week.
     * Notifications_26w: total number of notified cases with up to 26 weeks of delay.

    :param df: pandas.DataFrame of notifications with columns ['locality', 'epiyear', 'epiweek']
    :return: pandas.DataFrame
    """

    # Fill with all epiweeks:
    yearlist = list(df_in.epiyear.unique())
    # Remove NaN:
    yearlist = sorted([y for y in yearlist if y == y])
    lastweek = int(df_in.DT_DIGITA_epiweek[df_in.DT_DIGITA_epiyear == max(yearlist)].max())
    df = df_in.drop(columns=['DT_DIGITA_epiyear', 'DT_DIGITA_epiweek'])
    cols = ['UF', 'dado', 'epiyearweek', 'epiyear', 'epiweek', 'Notifications']
    dcols = ['d%s' % d for d in range(0, 27)]
    cols.extend(dcols)
    cols.extend(['Notifications_within_26w'])
    tbl = pd.DataFrame(columns=cols)
    tmpdict = []
    for dataset in df.dado.unique():
        for year in yearlist[:-1]:
            for week in range(1, (int(lastepiweek(year)) + 1)):
                tmpdict.extend([{'dado': dataset, 'epiyearweek': '%04dW%02d' % (year, week), 'epiyear': year,
                                 'epiweek': week}])
        year = yearlist[-1]
        tmpdict.extend([{'dado': dataset, 'epiyearweek': '%04dW%02d' % (year, week), 'epiyear': year, 'epiweek': week}
                        for week in range(1, (lastweek + 1))])
    dfallweeks = pd.DataFrame(tmpdict)

    for locality in ['UF', 'Regional', 'Regiao', 'Pais']:
        localitylist = sorted(df[locality].unique().tolist())
        dftmp = dfallweeks.copy()
        dftmp[locality] = localitylist[0]

        if len(localitylist) > 1:
            for loc in localitylist[1:]:
                dftmp = dftmp.append(dfallweeks, ignore_index=True, sort=False).fillna(loc)
        dftmp.sort_values(by=['dado', locality, 'epiyear', 'epiweek'], inplace=True)

        grp_cols = [locality, 'epiyearweek', 'epiyear', 'epiweek', 'dado']
        delay_table = df[grp_cols].groupby(grp_cols, as_index=False).size().reset_index()
        delay_table.rename(columns={0: 'Notifications'}, inplace=True)

        for k in range(0, 27):
            aux = df.loc[df.delayweeks == k, ].groupby(grp_cols, as_index=False).size().reset_index()
            aux.rename(columns={0: 'd%s' % k}, inplace=True)
            delay_table = delay_table.merge(aux, how='left', on=grp_cols).fillna(0)

        delay_table = dftmp[grp_cols].merge(delay_table, how='left', on=grp_cols).fillna(0)
        delay_table['Notifications_within_26w'] = delay_table[dcols].sum(axis=1)
        tbl = tbl.append(delay_table.rename(columns={locality: 'UF'}), ignore_index=True, sort=False)

    tbl['date'] = tbl[['epiyear', 'epiweek']].apply(lambda x: epiweek2date(x[0], x[1]), axis=1)

    return(tbl)


def main(fname, locality='UF'):

    module_logger.info('Read table from %s' % fname)
    df = readtable(fname)
    extract_quantile(df)
    df = delayimputation(df)
    df = createtable(df)
    for dataset in df.dado.unique():
        fout = '../clean_data/%s_sinpri2digita_table_weekly.csv' % dataset
        module_logger.info('Write table %s' % fout)
        df[df.dado == dataset].to_csv(fout, index=False)


if __name__ == '__main__':

    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        main(sys.argv[1], sys.argv[2])
