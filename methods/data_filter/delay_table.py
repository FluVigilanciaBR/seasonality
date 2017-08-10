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
    df = pd.read_csv(fname).rename(columns={'SG_UF_NOT': 'UF', 'DT_NOTIFIC_epiyear': 'epiyear',
                                            'DT_NOTIFIC_epiweek': 'epiweek'})
    df = df.loc[~np.isnan(df.UF), ]
    df.UF = df.UF.astype('int64')

    return(df)


def createtable(df=pd.DataFrame()):
    """Generate digitalization opportunity table by epidemiological week.

    From notification data, creates a table with total number of notified cases on column Notifications
    and number of cases introduced in the database by number of weeks since notification.

    The output is a pandas.DataFrame with columns:

    UF | epiyear | epiweek | Notifications | d0 | d1 | ... | d27 | Notifications_26w

    Where:

     * UF: has the Federation Unit code;
     * epiyear: epidemiological year;
     * epiweek: epidemiological week;
     * Notifications: total number of notified cases for that UF, epiyear, epiweek so far;
     * dn: corresponds to the number of cases digitalized n weeks *after* notification week.
     * Notifications_26w: total number of notified cases with up to 26 weeks of delay.

    :param df: pandas.DataFrame of notifications with columns ['UF', 'epiyear', 'epiweek']
    :return: pandas.DataFrame
    """

    # Fill with all epiweeks:
    yearlist = sorted(list(df.epiyear.unique()))
    print(yearlist)
    lastweek = df.epiweek[df.epiyear == max(yearlist)].max()
    uflist = list(df.UF.unique())
    tmpdict = []
    for year in yearlist[:-1]:
        for week in range(1, (int(episem.lastepiweek(year))+1)):
            for uf in uflist:
                tmpdict.extend([{'UF': uf, 'epiyear': year, 'epiweek': week}])
    tmpdict.extend([{'UF': uf, 'epiyear': yearlist[-1], 'epiweek': week} for week in
                    range(1,(lastweek+1)) for uf in uflist])
    dftmp = pd.DataFrame(tmpdict)

    grp_cols = ['UF', 'epiyear', 'epiweek']
    delay_table = df[grp_cols].groupby(grp_cols, as_index=False).size().reset_index()
    delay_table.rename(columns={0: 'Notifications'}, inplace=True)

    for k in range(0, 27):
        aux = df.loc[df.DelayWeeks == k, ].groupby(grp_cols, as_index=False).size().reset_index()
        aux.rename(columns={0: 'd%s' % k}, inplace=True)
        delay_table = delay_table.merge(aux, how='left', on=grp_cols).fillna(0)

    delay_table = dftmp.merge(delay_table, how='left', on=grp_cols).fillna(0)
    delay_table['Notifications_26w'] = delay_table[[k for k in range(4,31)]].sum(axis=1)
    return(delay_table)


def main(fname):

    df = readtable(fname)
    df = createtable(df)
    df.to_csv(fname[:-4]+'_delay_table.csv', index=False)


if __name__ == '__main__':
    main(sys.argv[1])