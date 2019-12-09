# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'
'''
Convert dfb file to csv format
'''

import argparse
import logging
import pandas as pd
from argparse import RawDescriptionHelpFormatter
from dbfread import DBF

module_logger = logging.getLogger('update_system.data_filter.dbf2csv')


def dbf2csv(fin, fout):

    for enc in ['utf-8', 'utf-16', 'latin-1']:
        try:
            table = DBF(fin, encoding=enc)
            df = pd.DataFrame(iter(table))
            print('yep: %s' % enc)
            break
        except UnicodeDecodeError:
            print('nope: %s' % enc)
            pass

    df.to_csv(fout, encoding='utf-8')

    return


def main(flist):

    for fname in flist:
        module_logger.info('DBF2CSV: PROCESSING %s' % fname)
        fout = '.'.join(fname.split('.')[:-1]) + '.csv'
        dbf2csv(fname, fout)
        module_logger.info('DBF2CSV: CONVERTED TO %s' % fout)



if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Convert DBF file to CSV.\n" +
                                                 "python3 dbf2csv.py --path ../data/influ*.DBF\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--path', nargs='*', action='append', help='Path to data file')
    args = parser.parse_args()
    main(args.path[0])
