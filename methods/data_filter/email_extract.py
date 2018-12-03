__author__ = 'Marcelo Ferreira da Costa Gomes'

import glob
import os
import argparse
import logging
import pandas as pd
from argparse import RawDescriptionHelpFormatter
from subprocess import call

module_logger = logging.getLogger('update_system.email_extract')


def extract_csv(dir):
    cwdir = os.getcwd()
    os.chdir(dir)

    for f in glob.glob('*.zip'):
        call(["rename", 's/ //', f])

    file = sorted(glob.glob('*.zip'), reverse=True)[-1]
    os.chdir(cwdir)
    return pd.read_csv('%s/%s' %(dir, file), header=1, encoding='utf-16')


def write_to_folder(df, year):

    df.to_csv('../data/INFLUD%s.csv' % year, index=False, encoding='utf-8')
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
