__author__ = 'Marcelo Ferreira da Costa Gomes'

import glob
import os
import argparse
import logging
import zipfile
import pandas as pd
from argparse import RawDescriptionHelpFormatter
from datetime import date
from dbfread import DBF
from fnmatch import fnmatch
from patoolib import extract_archive
from subprocess import run

module_logger = logging.getLogger('update_system.email_extract')


def write_to_folder(df, year, dir=None):

    if not dir:
        dir = os.getcwd()

    output = os.path.join(dir, '..', 'data', 'INFLUD%s.csv' % year)
    df.to_csv(output, index=False, encoding='utf-8')
    return


def extract_csv(dir, year=None):
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
            extract_archive(f)
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

    for file in flist:
        module_logger.info('Processing file: %s' % file)
        y = file.split('.')[-2][-4:]
        module_logger.info('Epidemiological year: %s' % y)
        if year and y != year:
            pass

        if 'csv' in file.lower():
            for enc in ['utf-8', 'utf-16', 'latin-1']:
                try:
                    df = pd.read_csv(file, header=0, encoding=enc, low_memory=False)
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

        write_to_folder(df, y, cwd)
        run(['rm', '-f', dir+file], check=True)

    os.chdir(cwd)
    return df


def main(dir, year=None):

    df = extract_csv(dir, year)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Extract csv from zipped file sent by email.\n" +
                                                 "python3 email_extract.py --dir [folder] --year [YYYY]\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--dir', help='Base folder')
    parser.add_argument('--year', help='Base year')
    args = parser.parse_args()
    print(args)
    main(args.dir, args.year)
