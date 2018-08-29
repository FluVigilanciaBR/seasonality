__author__ = 'Marcelo Ferreira da Costa Gomes'

import os, logging, glob, argparse
from argparse import RawDescriptionHelpFormatter
from data_filter import dbf2csv,\
    sinan_filter_of_interest,\
    insert_epiweek,\
    delay_datasets, \
    sinan_convert2mem,\
    consolidate_datasets
from mem import sinan_mem_inset_thresholds
from subprocess import run

logger = logging.getLogger('update_system')
logger.setLevel(logging.DEBUG)
fh = logging.FileHandler('InfoGripe_system_update.log')
ch = logging.FileHandler('InfoGripe_system_update.error.log')
ch.setLevel(logging.ERROR)
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
fh.setFormatter(formatter)
ch.setFormatter(formatter)
logger.addHandler(fh)
logger.addHandler(ch)


def convert_dbf(flist):
    logger.info('calling dbf2csv module')
    dbf2csv.main(flist)
    logger.info('dbf2csv module: DONE')

    return


def apply_filters():
    logger.info('calling sinan_filter_of_interest module')
    flist = glob.glob('../data/INFLUD*.csv')
    sinan_filter_of_interest.main(flist)
    logger.info('sinan_filter_of_interest module: DONE')

    return


def add_epiweek():
    flist = ['clean_data_srag.csv', 'clean_data_sragflu.csv', 'clean_data_obitoflu.csv']
    for fname in flist:
        logger.info('Inserting epiweek on file %s' % fname)
        insert_epiweek.main(fname)
        logger.info('... DONE')

    return


def convert2mem():
    flist = ['../clean_data/clean_data_srag_epiweek.csv',
             '../clean_data/clean_data_sragflu_epiweek.csv',
             '../clean_data/clean_data_obitoflu_epiweek.csv']
    for fname in flist:
        logger.info('Converting to MEM structure: %s' % fname)
        sinan_convert2mem.main(fname)
        logger.info('... DONE')

    return


def apply_mem():
    dataset = ['srag', 'sragflu', 'obitoflu']
    for data in dataset:
        fname = '../clean_data/clean_data_%s_epiweek4mem-incidence.csv' % data
        logger.info('Calculating MEM thresholds for dataset: %s' % data)
        sinan_mem_inset_thresholds.main(fname)

        os.rename('../clean_data/mem-report.csv',
                  '../clean_data/%s_mem-report.csv' % data)
        os.rename('../clean_data/mem-typical.csv',
                  '../clean_data/%s_mem-typical.csv' % data)

        logger.info('... DONE')

    return


def apply_estimator():
    dataset = ['srag', 'sragflu', 'obitoflu']
    Rscript = 'chainladder_inla_Influenza_simples_v1.R'

    for data in dataset:
        logger.info('Calculating estimates for dataset: %s' % data)
        run(['Rscript', '--vanilla', Rscript, '-d', 'max', '-t', data])
        logger.info('... DONE')

    return


def main(flist, update_mem = False):
    '''
    Run all scripts to update the system with new database.
    Optional: update MEM thresholds

    :param update_mem:
    :return:
    '''

    logger.info('Convert DBF to CSV')
    convert_dbf(flist)
    logger.info('Convert DBF to CSV: DONE')

    print('Now what?')
    os.chdir('./data_filter')
    print(os.getcwd())

    logger.info('Aggregate and filter data')
    apply_filters()
    logger.info('Aggregate and filter data: DONE')

    logger.info('Insert epiweek')
    add_epiweek()
    logger.info('Insert epiweek: DONE')

    logger.info('Create table of opportunities')
    delay_datasets.main()
    logger.info('Create table of opportunities: DONE')

    logger.info('Convert to MEM structure and aggregate by epiweek')
    convert2mem()
    logger.info('Convert to MEM structure and aggregate by epiweek: DONE')

    if update_mem:
        os.chdir('../mem')
        logger.info('Apply MEM')
        apply_MEM()
        logger.info('Apply MEM: DONE')

    # os.chdir('../opportunity_estimator')
    # logger.info('Apply opportunity estimator')
    # apply_estimator()
    # logger.info('Apply opportunity estimator: DONE')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Update InfoGripe database.\n" +
                                                 "python3 update_system.py --mem --path ./data/influ*.DBF\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--mem', action='store_true', help='Update MEM thresholds.')
    parser.add_argument('--path', nargs='*', action='append', help='Path to data file')
    args = parser.parse_args()
    main(args.path[0], args.mem)
