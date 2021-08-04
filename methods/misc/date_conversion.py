# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import logging
import pandas as pd


module_logger = logging.getLogger('update_system.date_conversion')


def remove_whitespace(x):
    if isinstance(x, object):
        x = x.str.lstrip().str.rstrip()
        x = x.where(lambda v: v != '', None)
        return x
    else:
        return x


def date_cleanup(df, dt_cols):
    '''
    Standardize column data and discard those without notification and/or first symptoms date.
    :param df: notifications data frame
    :param dt_cols: list of data columns
    :return: df: standardized data frame
    '''
    module_logger.info('Date columns cleanup: %s' % dt_cols)

    # Filter by notification date
    df = df.where(df != -1, None)
    df[dt_cols] = df[dt_cols].where(df[dt_cols] != 10101, None)

    # Convert all date related columns to datetime format
    for col in dt_cols:
        # Convert all date columns to datetime format. Output will have the format YYYY-MM-DD
        dtformat = '%Y%m%d'
        sample = df.loc[~pd.isnull(df[col]), col].values[0]
        if isinstance(sample, str):
            df[col] = remove_whitespace(df[col])

        if sum(~pd.isnull(df[col])) > 0:
            sample = df.loc[~pd.isnull(df[col]), col].values[0]
            if isinstance(sample, str):
                if 'T' in sample or any(df[col].str.contains('T')):
                    df[col] = pd.to_datetime(df[col].str[:10], errors='coerce', utc=True).dt.date
                else:
                    dtsep = '-'
                    if '/' in sample:
                        dtsep = '/'
                    dttest = pd.DataFrame(list(
                        df.loc[~pd.isnull(df[col]), col].str.split(dtsep)
                    ))
                    maxvals = [int(dttest[i].max()) for i in range(3)]
                    del dttest
                    yearpos = maxvals.index(max(maxvals))
                    if yearpos == 2:
                        if maxvals[1] > 12:
                            dtformat = '%m' + dtsep + '%d' + dtsep + '%Y'
                        else:
                            dtformat = '%d' + dtsep + '%m' + dtsep + '%Y'
                    else:
                        dtformat = '%Y' + dtsep + '%m' + dtsep + '%d'
                    df[col] = pd.to_datetime(df[col], errors='coerce', format=dtformat)
            else:
                df[col] = pd.to_datetime(df[col], errors='coerce', format=dtformat)
        else:
            df[col] = pd.to_datetime(df[col])

    return df
