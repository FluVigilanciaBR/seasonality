__author__ = 'Marcelo Ferreira da Costa Gomes'

import os, logging, glob, argparse
from argparse import RawDescriptionHelpFormatter
from subprocess import run

logger = logging.getLogger('update_system')
logger.setLevel(logging.DEBUG)
fh = logging.FileHandler('InfoGripe_system_update.log')
ch = logging.StreamHandler('InfoGripe_system_update.error.log')
ch.setLevel(logging.ERROR)
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
fh.setFormatter(formatter)
ch.setFormatter(formatter)
logger.addHandler(fh)
logger.addHandler(ch)


# TODO: insert module for delay_table.py script
# TODO: update opportunity estimator module


def convert_dbf(flist):
    from data_filter import dbf2csv

    module_name = dbf2csv.__name__
    try:
        dbf2csv.main(flist)
    except:
        logger.exception(module_name)
        raise

    logger.info('%s : DONE', module_name)
    return


def apply_filters(flist=None):
    from data_filter import sinan_filter_of_interest

    module_name = sinan_filter_of_interest.__name__

    if not flist:
        flist = sorted(glob.glob('../data/INFLUD*.csv'))

    try:
        sinan_filter_of_interest.main(flist)
    except:
        logger.exception(module_name)
        raise

    logger.info('%s : DONE', module_name)

    return


def add_epiweek():
    from data_filter import insert_epiweek

    module_name = insert_epiweek.__name__
    flist = ['clean_data_srag.csv', 'clean_data_sragflu.csv', 'clean_data_obitoflu.csv']
    for fname in flist:
        logger.info('Inserting epiweek on file %s' % fname)
        try:
            insert_epiweek.main(fname)
        except:
            logger.exception(module_name)
            raise

        logger.info('... DONE')

    logger.info('%s : DONE', module_name)
    return


def convert2mem():
    from data_filter import sinan_convert2mem

    module_name = sinan_convert2mem.__name__
    flist = ['../clean_data/clean_data_srag_epiweek.csv',
             '../clean_data/clean_data_sragflu_epiweek.csv',
             '../clean_data/clean_data_obitoflu_epiweek.csv']
    for fname in flist:
        logger.info('Converting to MEM structure: %s' % fname)
        try:
            sinan_convert2mem.main(fname)
        except:
            logger.exception(module_name)
            raise

        logger.info('... DONE')

    logger.info('%s : DONE', module_name)
    return


def apply_mem():
    from mem import sinan_mem_inset_thresholds

    module_name = sinan_mem_inset_thresholds.__name__
    dataset = ['srag', 'sragflu', 'obitoflu']
    for data in dataset:
        fname = '../clean_data/clean_data_%s_epiweek4mem-incidence.csv' % data
        logger.info('Calculating MEM thresholds for dataset: %s' % data)
        try:
            sinan_mem_inset_thresholds.main(fname)
        except:
            logger.exception(module_name)
            raise

        os.rename('../clean_data/mem-report.csv',
                  '../clean_data/%s_mem-report.csv' % data)
        os.rename('../clean_data/mem-typical.csv',
                  '../clean_data/%s_mem-typical.csv' % data)

        logger.info('... DONE')

    logger.info('%s : DONE', module_name)
    return


def apply_estimator():
    from opportunity_estimator import add_situation2weekly_data

    dataset = ['srag', 'sragflu', 'obitoflu']
    Rscript = 'chainladder_inla_Influenza_simples_v1.R'

    for data in dataset:
        logger.info('Calculating estimates for dataset: %s' % data)
        try:
            run(['Rscript', '--vanilla', Rscript, '-d', 'max', '-t', data])
        except:
            logger.exception('opportunity_estimator.chainladder_inla_Influenza_simples_v1.R')
            raise

        logger.info('Adding situation info for dataset: %s' % data)
        try:
            add_situation2weekly_data.main([data])
        except:
            logger.exception(add_situation2weekly_data.__name__)
            raise

        logger.info('... DONE')

    logger.info('opportunity_estimator.chainladder_inla_Influenza_simples_v1 : DONE')
    return


def consolidate():
    from data_filter import consolidate_datasets

    module_name = consolidate_datasets.__name__
    try:
        consolidate_datasets.main(True)
    except:
        logger.exception(module_name)
        raise

    logger.info('%s : DONE', module_name)
    return


def main(flist = None, update_mem = False, module_list = None, history_files=None):
    '''
    Run all scripts to update the system with new database.
    Optional: update MEM thresholds

    :param update_mem:
    :return:
    '''
    if module_list and 'all' in module_list:
        module_list = ['dbf2csv',
                       'filter',
                       'epiweek',
                       'opportunities',
                       'convert2mem',
                       'estimator',
                       'consolidate']

    logger.info('System update: START')
    logger.info('Update MEM: %s', update_mem)
    logger.info('File list: %s', flist)
    logger.info('Update modules: %s', module_list)
    logger.info('Historical files: %s', history_files)

    if 'dbf2csv' in module_list:
        logger.info('Convert DBF to CSV')
        convert_dbf(flist)

    os.chdir('./data_filter')

    if 'filter' in module_list:
        logger.info('Aggregate and filter data')
        apply_filters(history_files)

    if 'epiweek' in module_list:
        logger.info('Insert epiweek')
        add_epiweek()

    if 'opportunities' in module_list:
        from data_filter import delay_datasets

        logger.info('Create table of opportunities')
        delay_datasets.main()

    if 'convert2mem' in module_list:
        logger.info('Convert to MEM structure and aggregate by epiweek')
        convert2mem()

    os.chdir('../mem')
    if update_mem:
        logger.info('Apply MEM')
        apply_mem()

    os.chdir('../opportunity_estimator')
    if 'estimator' in module_list:
        logger.info('Apply opportunity estimator')
        apply_estimator()

    os.chdir('../data_filter')
    if 'consolidate' in module_list:
        logger.info('Consolidate dataset and update DB')
        consolidate()

    logger.info('System update: DONE')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Update InfoGripe database.\n" +
                                                 "python3 update_system.py --mem --path ./data/influ*.DBF\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--mem', action='store_true', help='Update MEM thresholds.')
    parser.add_argument('--modules', nargs='*', action='append', help='Which modules should be ran.',
                        default=[])
    parser.add_argument('--path', nargs='*', action='append', help='Path to data file',
                        default=None)
    parser.add_argument('--history', nargs='*', action='append', help='Path to historical notifications csv files',
                        default=None)
    args = parser.parse_args()
    if args.path:
        args.path = args.path[0]
    if args.modules:
        args.modules = [x.lower() for x in args.modules[0]]
    if args.history:
        args.history = args.history[0]

    main(args.path, args.mem, args.modules, args.history)
