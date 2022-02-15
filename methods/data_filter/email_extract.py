__author__ = 'Marcelo Ferreira da Costa Gomes'

import glob
import os
import argparse
import logging
import re
import zipfile
import pandas as pd
from argparse import RawDescriptionHelpFormatter
from datetime import date
from dbfread import DBF
from fnmatch import fnmatch
from patoolib import extract_archive
from subprocess import run

module_logger = logging.getLogger('update_system.email_extract')


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
                if 'T' in sample:
                    df[col] = pd.to_datetime(df[col].str[:10], errors='coerce', format='%Y-%m-%d')
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


def write_to_folder(df, year, dir=None, append=False):

    if not dir:
        dir = os.getcwd()

    output = os.path.join(dir, '..', 'data', 'INFLUD%s.csv' % year)
    if append:
        df = pd.concat([df, pd.read_csv(output, dtype=df.dtypes.to_dict())], ignore_index=True, sort=False)
    df.columns = df.columns.str.upper()
    df.to_csv(output, index=False, encoding='utf-8', date_format='%Y-%m-%d')
    return


def extract_csv(dir, sep=',', year=None):
    cwd = os.getcwd()
    os.chdir(dir)

    today = date.today().strftime('%Y-%m-%d')
    for f in glob.glob('*.zip'):
        run(["rename", 's/ //', f], check=True)

    try:
        fz = sorted(glob.glob('*.zip'), reverse=True)
        fz.extend(glob.glob('*.rar'))
        for f in fz:
            module_logger.info('Extracting files from: %s' % f)
            extract_archive(f, outdir='./')
            run(['mv', '-f', f, './processed/%s_%s' % (today, f)], check=True)

    except ValueError as error:
        module_logger.error(error)
        raise ValueError('No zip or rar file on %s' % dir)

    if glob.glob('*.csv'):
        flist = glob.glob('*.csv')
        suff = '.csv'
    elif glob.glob('*.CSV'):
        flist = glob.glob('*.CSV')
        suff = '.CSV'
    elif glob.glob('*.dbf'):
        flist = glob.glob('*.dbf')
        suff = '.dbf'
    elif glob.glob('*.DBF'):
        flist = glob.glob('*.DBF')
        suff = '.DBF'
    elif glob.glob('SRAGHOSP*'):
        flist = [f for f in glob.glob("SRAGHOSP*") if not fnmatch(f, "*.zip")]

    yold = -1
    for file in flist:
        module_logger.info('Processing file: %s' % file)
        y = file.split('.')[-2][-4:]
        if y == yold:
            append = True
        else:
            append = False
        yold = y
        module_logger.info('Epidemiological year: %s' % y)
        if year and y != year:
            pass

        if 'csv' in file.lower():
            for enc in ['utf-8', 'utf-16', 'latin-1']:
                try:
                    df = pd.read_csv(file, sep=sep, header=0, encoding=enc, low_memory=False)
                    break
                except UnicodeDecodeError:
                    pass
        else:
            for enc in ['utf-8', 'utf-16', 'latin-1']:
                try:
                    table = DBF(file, encoding=enc)
                    df = pd.DataFrame(iter(table))
                    break
                except UnicodeDecodeError:
                    pass

        df.columns = df.columns.str.upper()
        tgtcols = list(df.columns)
        regexp = re.compile('^DT')
        dt_cols = list(filter(regexp.match, tgtcols))
        df = date_cleanup(df, dt_cols)
        write_to_folder(df, y, cwd, append=append)
        run(['rm', '-f', dir+file], check=True)

    os.chdir(cwd)
    return df


def main(dir, sep=',', year=None):

    df = extract_csv(dir, sep=sep, year=year)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Extract csv from zipped file sent by email.\n" +
                                                 "python3 email_extract.py --dir [folder] --year [YYYY]\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--dir', help='Base folder')
    parser.add_argument('--year', help='Base year')
    args = parser.parse_args()
    print(args)
    main(args.dir, args.year)
