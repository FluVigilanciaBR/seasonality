__author__ = 'Marcelo Ferreira da Costa Gomes'

import pandas as pd
import numpy as np
import episem
import sys


def readtable(fname):
    """Read notification table.

    :param fname: path to file
    :return: pandas.DataFrame
    """
    df = pd.read_csv(fname, low_memory=False)[['SG_UF', 'ID_MN_RESI', 'DT_NOTIFIC_epiyear',
                                               'DT_NOTIFIC_epiweek',
                                               'Notific2Digita_DelayWeeks']].rename(columns={'SG_UF': 'uf',
                                                                                             'ID_MN_RESI': 'municipio',
                                                                                             'DT_NOTIFIC_epiyear': 'epiyear',
                                                                                             'DT_NOTIFIC_epiweek': 'epiweek'})
    df = df[df.uf == 35].copy()
    dfmunreg = pd.read_csv('~/dropbox-personal/Dropbox/IBGE/rl_municip_regsaud.csv', sep=';', header=None,
                           names=['municipio', 'regionalsaude'])
    dfmunreg.set_index('municipio', inplace=True)
    df['regionalsaude'] = df.municipio.map(dfmunreg.regionalsaude)

    return(df)


def createtable(df=pd.DataFrame(), locality='uf'):
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
    yearlist = sorted(list(df.epiyear.unique()))
    print(yearlist)
    lastweek = df.epiweek[df.epiyear == max(yearlist)].max()
    localitylist = list(df[locality].unique())
    tmpdict = []
    for year in yearlist[:-1]:
        for week in range(1, (int(episem.lastepiweek(year))+1)):
            for loc in localitylist:
                tmpdict.extend([{locality: loc, 'epiyear': year,
                                 'epiweek': week}])
    tmpdict.extend([{locality: loc, 'epiyear': yearlist[-1], 'epiweek': week}
                    for week in range(1,(lastweek+1)) for loc in localitylist])
    dftmp = pd.DataFrame(tmpdict)

    grp_cols = [locality, 'epiyear', 'epiweek']
    delay_table = df[grp_cols].groupby(grp_cols, as_index=False).size().reset_index()
    delay_table.rename(columns={0: 'Notifications'}, inplace=True)

    for k in range(0, 27):
        aux = df.loc[df.Notific2Digita_DelayWeeks == k, ].groupby(grp_cols, as_index=False).size().reset_index()
        aux.rename(columns={0: 'd%s' % k}, inplace=True)
        delay_table = delay_table.merge(aux, how='left', on=grp_cols).fillna(0)

    delay_table = dftmp.merge(delay_table, how='left', on=grp_cols).fillna(0)
    delay_table['Notifications_within_26w'] = delay_table[[k for k in range(4,31)]].sum(axis=1)
    return(delay_table)


def main(fname, locality='uf'):

    df = readtable(fname)
    df = createtable(df, locality)
    df.to_csv(fname[:-4]+'_delay_table_SP.csv', index=False)


if __name__ == '__main__':

    if len(sys.argv) == 2:
        main(sys.argv[1])
    else:
        main(sys.argv[1], sys.argv[2])
