__author__ = 'Marcelo Ferreira da Costa Gomes'

import glob
import os
import argparse
import logging
import pandas as pd
from argparse import RawDescriptionHelpFormatter
from subprocess import run
from datetime import date

module_logger = logging.getLogger('update_system.email_extract')

def extract_csv(dir):
    cwd = os.getcwd()
    os.chdir(dir)

    for f in glob.glob('*.zip'):
        run(["rename", 's/ //', f], check=True)

    try:
        file = sorted(glob.glob('*.zip'), reverse=True)[-1]
    except ValueError as error:
        module_logger.error(error)
        raise ValueError('No zip file on %s' % dir)
    df = pd.read_csv(file, header=1, encoding='utf-16')
    today = date.today().strftime('%Y-%m-%d')
    run(['mv', '-f', file, './processed/%s_%s' % (today, file)], check=True)
    os.chdir(cwd)
    return df


def write_to_folder(df, year):

    output = os.path.join(os.getcwd(), '..', 'data', 'INFLUD%s.csv' % year)
    df.to_csv(output, index=False, encoding='utf-8')
    return


def main(dir, year):

    df = extract_csv(dir)
    write_to_folder(df, year)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Extract csv from zipped file sent by email.\n" +
                                                 "python3 email_extract.py --dir [folder] --year [YYYY]\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--dir', help='Base folder')
    parser.add_argument('--year', help='Base year')
    args = parser.parse_args()
    print(args)
    main(args.dir, args.year)
