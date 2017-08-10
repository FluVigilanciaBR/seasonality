# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'
'''
Convert dfb file to csv format
'''

import csv
import argparse
from argparse import RawDescriptionHelpFormatter
from dbfread import DBF


def dbf2csv(fin, fout):
    table = DBF(fin)
    writer = csv.writer(open(fout,'w'))

    writer.writerow(table.field_names)
    i = 1
    for record in table:
        print(i)
        writer.writerow(list(record.values()))
        i += 1

    return


def main(flist):

    for fname in flist:
        print(fname)
        fout = '.'.join(fname.split('.')[:-1]) + '.csv'
        dbf2csv(fname, fout)

    exit()


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Convert DBF file to CSV.\n" +
                                                 "python3 dbf2csv.py --path ../data/influ*.DBF\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--path', nargs='*', action='append', help='Path to data file')
    args = parser.parse_args()
    main(args.path[0])
