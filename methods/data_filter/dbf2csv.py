# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'
'''
Convert dfb file to csv format
'''

import csv
import argparse
import logging
from argparse import RawDescriptionHelpFormatter
from dbfread import DBF

module_logger = logging.getLogger('update_system.dbf2csv')

def dbf2csv(fin, fout):
    table = DBF(fin)
    writer = csv.writer(open(fout,'w'))

    writer.writerow(table.field_names)
    for record in table:
        writer.writerow(list(record.values()))

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
