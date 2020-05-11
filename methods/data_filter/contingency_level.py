# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import pandas as pd
import numpy as np
from fludashboard.libs.flu_data import FluDB

db = FluDB()


def get_all_territories_and_years():
    df = db.read_data(
        table_name='current_estimated_values',
        dataset_id=1, scale_id=1, territory_id=0
    )
    list_of_years = list(set(df.epiyear))
    with db.conn.connect() as conn:
        sql = '''
        SELECT id FROM territory
        '''
        list_of_territories = list(set(pd.read_sql(sql, conn).id))

    df = pd.DataFrame(
        [
            {'territory_id': t, 'epiyear': y} for t in set(list_of_territories) - {9, 99, 9999}
            for y in list_of_years
        ]
    )

    return df


def contingency_trigger(dataset_id: int, year: int, territory_id: int):
    """

    :param dataset_id:
    :param year:
    :param territory_id:
    :return:
    """
    df = db.get_data(
        dataset_id=dataset_id, scale_id=1, year=year,
        territory_id=territory_id
    )[['estimated_cases', 'typical_median', 'typical_low', 'typical_high']]

    # get_data stores the difference between typical levels in each column:
    df.typical_high += df.typical_median + df.typical_low

    # If not obitoflu dataset (3), uses last 4 weeks, o.w. use 3:
    if dataset_id < 3:
        wdw = 4
    else:
        wdw = 3

    weeks = df.shape[0]
    alert_zone = False
    data_increase = False
    if weeks < wdw+1:
        alert_zone = False
        data_increase = False
    else:
        for i in range(wdw+1, weeks+1):
            alert_zone = any(df.estimated_cases[(i-wdw):i] > df.typical_high[(i-wdw):i])
            data_increase = all(
                df.estimated_cases[(i-wdw):i].values -
                df.estimated_cases[(i-wdw - 1):(i-1)].values > 0
            )
            if alert_zone & data_increase:
                return alert_zone & data_increase, i-1

    return alert_zone & data_increase, 1


def check_contingency_decrease(year: int, territory_id: int, cont_level: int, week: int):
    dataset_id = cont_level - 1
    df = db.get_data(
        dataset_id=dataset_id, scale_id=1, year=year,
        territory_id=territory_id
    ).loc[lambda dftmp: (dftmp['situation_id'] == 2) | (dftmp['situation_id'] == 3),
          ['estimated_cases', 'typical_median', 'typical_low']]
    # get_data stores the difference between typical levels in each column:
    df.typical_median += df.typical_low

    weeks = df.shape[0]
    for i in range(week, weeks):
        if all(df.estimated_cases[i:(i+2)] <= df.typical_median[i:(i+2)]):
            cont_level -= 1
            week = i+1
            if cont_level > 1:
                check_contingency_decrease(year=year, territory_id=territory_id, cont_level=cont_level, week=week)
            else:
                break

    return cont_level


def contingency_level(year: int, territory_id: int, maximum=False):
    for dataset_id in range(3, 0, -1):
        alert, week = contingency_trigger(dataset_id=dataset_id, year=year, territory_id=territory_id)
        if alert & (not maximum):
            return check_contingency_decrease(year=year, territory_id=territory_id,
                                              cont_level=(dataset_id+1), week=week)
        elif alert:
            return(dataset_id+1)

    return (1)


def calc_weekly_alert_level(se: pd.Series):

    _max = max([
        se.very_high_level, se.high_level, se.epidemic_level, se.low_level
    ])

    return (
        0 if np.isnan(_max) else
        1 if se.low_level == _max else
        2 if se.epidemic_level == _max else
        3 if se.high_level == _max else
        4
    )


def apply_filter_alert_by_epiweek(year: int, territory_id: int):

    df = pd.DataFrame()
    for dataset_id in range(1, 7):
        df = df.append(
            db.get_data(
                dataset_id=dataset_id, scale_id=1, year=year, territory_id=territory_id
            )[['dataset_id', 'territory_id', 'epiyear', 'epiweek', 'low_level', 'epidemic_level', 'high_level',
               'very_high_level', 'situation_id']],
            ignore_index=True
        )
        if year > 2009:
            df = df.append(
                db.get_data(
                    dataset_id=dataset_id, scale_id=1, year=year-1, territory_id=territory_id
                ).loc[lambda df: df.epiweek == max(df.epiweek[~df.situation_id.isin([1, 4])]),
                      ['dataset_id', 'territory_id', 'epiyear', 'epiweek',
                       'low_level', 'epidemic_level', 'high_level',
                       'very_high_level', 'situation_id']],
                ignore_index=True
            )

    epiweek = max(df.epiweek[~df.situation_id.isin([1, 4]) & (df.epiyear == year)])
    df.loc[(df.situation_id.isin([1, 4])) & (df.epiweek > epiweek - 2) & (df.epiyear == year),
           ['low_level', 'epidemic_level', 'high_level', 'very_high_level']] = None
    df['alert'] = df.apply(calc_weekly_alert_level, axis=1)
    df.sort_values(by=['territory_id', 'dataset_id', 'epiyear', 'epiweek'], inplace=True)
    df.loc[df.alert == 0, 'alert'] = None
    df.alert = df.alert.fillna(method='ffill').astype(int)
    df = df[df.epiyear == year]

    return df[['dataset_id', 'territory_id', 'epiyear', 'epiweek', 'alert', 'low_level', 'epidemic_level',
               'high_level', 'very_high_level']]


def weekly_alert_table_all(df):
    df_alert = pd.DataFrame()

    for territory_id in sorted(df.territory_id.unique()):
        for epiyear in sorted(df.epiyear.unique()):
            df_alert = df_alert.append(apply_filter_alert_by_epiweek(year=epiyear, territory_id=territory_id),
                                       ignore_index=True)

    return df_alert


def season_alert_level(se):

    alert_counts = se.value_counts()
    if max(alert_counts.index) in [3, 4]:
        try:
            high_threshold = alert_counts[3] + alert_counts[4]
        except:
            high_threshold = alert_counts[alert_counts.index[-1]]
    else:
        high_threshold = 0

    return (
        4 if high_threshold >= 5 else
        3 if high_threshold >= 1 else
        2 if 2 in alert_counts.index else
        1
    )


def calc_season_contingency():

    df_contingency = get_all_territories_and_years()
    df_contingency['contingency'] = df_contingency.apply(lambda x: contingency_level(year=x.epiyear,
                                                                                     territory_id=x.territory_id),
                                                         axis=1)
    df_contingency['contingency_max'] = df_contingency.apply(lambda x: contingency_level(year=x.epiyear,
                                                                                         territory_id=x.territory_id,
                                                                                         maximum=True), axis=1)

    return df_contingency


def calc_season_alert(df_alert_weekly):

    df_alert_season = df_alert_weekly[['dataset_id', 'territory_id', 'epiyear']].drop_duplicates()
    df_alert_season['season_level'] = df_alert_season.apply(lambda se: season_alert_level(
        df_alert_weekly.alert[
            (df_alert_weekly.dataset_id == se.dataset_id) &
            (df_alert_weekly.territory_id == se.territory_id) &
            (df_alert_weekly.epiyear == se.epiyear)]
        ), axis=1)

    return df_alert_season
