from typing import Dict

__author__ = 'Marcelo Ferreira da Costa Gomes'

import os, logging, glob, argparse
from argparse import RawDescriptionHelpFormatter
from subprocess import run
import smtplib
import yaml
import datetime

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

home_path = os.path.expanduser("~")
settings_path = os.path.join(home_path, '.seasonality.yaml')

if os.path.exists(settings_path):
    EMAIL = {
        'NAME': None,
        'USER': None,
        'PASSWORD': None,
        'TO': None,
    }
    with open(os.path.join(settings_path), 'r') as f:
        globals().update(yaml.load(f))

    mail_error = {
        'subject': "InfoGripe Updater: error log",
        'email_body': """
        This is an automated message from InfoGripe Updater.
        During system's database update at %(time)s, there was an error at module %(mdl_name)s.
        Please check the log for details.

        All the best,
        InfoGripe Updater Monitor. 
        """
    }
    mail_success = {
        'subject': "InfoGripe Updater: success",
        'email_body': """
        This is an automated message from InfoGripe Updater.
        System's database update ran without raising any errors at %(time)s.

        All the best,
        InfoGripe Updater Monitor. 
        """
    }
    send_email = """
    From: %(USER)s
    To: %(TO)s
    Subject: %(subject)s
               
    %(email_body)s
    """
else:
    logger.exception('Please configure email settings for system updater at (%s)' % settings_path)
    raise Exception

try:
    server = smtplib.SMTP_SSL('smtp.gmail.com', 465)
    server.ehlo()
    server.login(EMAIL['USER'], EMAIL['PASSWORD'])
except Exception as exception:
    logger.exception(exception)
    raise

time = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')


def convert_dbf(flist):
    from data_filter import dbf2csv

    module_name = dbf2csv.__name__
    try:
        dbf2csv.main(flist)
    except:
        logger.exception(module_name)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        server.sendmail(EMAIL['USER'], EMAIL['TO'], send_email % {**EMAIL, **mail_error})
        raise

    logger.info('%s : DONE', module_name)
    return


def email_update(dir, years):
    from data_filter import email_extract

    module_name = email_extract.__name__

    try:
        for year in years:
            logger.info('Updating over e-mail. Base year: %s' % year)
            email_extract.main(dir, year)
    except:
        logger.exception(module_name)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        server.sendmail(EMAIL['USER'], EMAIL['TO'], send_email % {**EMAIL, **mail_error})
        raise

    logger.info('%s : DONE', module_name)
    return


def apply_filters(flist=None):
    from data_filter import sinan_filter_of_interest

    module_name = sinan_filter_of_interest.__name__

    if not flist:
        flist = sorted(glob.glob('../data/INFLUD*.csv'))

    logger.info('Historical files: %s', flist)
    try:
        sinan_filter_of_interest.main(flist)
    except:
        logger.exception(module_name)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        server.sendmail(EMAIL['USER'], EMAIL['TO'], send_email % {**EMAIL, **mail_error})
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
            mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
            server.sendmail(EMAIL['USER'], EMAIL['TO'], send_email % {**EMAIL, **mail_error})
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
            mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
            server.sendmail(EMAIL['USER'], EMAIL['TO'], send_email % {**EMAIL, **mail_error})
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
            mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
            server.sendmail(EMAIL['USER'], EMAIL['TO'], send_email % {**EMAIL, **mail_error})
            raise

        os.rename('../clean_data/mem-report.csv',
                  '../clean_data/%s_mem-report.csv' % data)
        os.rename('../clean_data/mem-typical.csv',
                  '../clean_data/%s_mem-typical.csv' % data)

        logger.info('... DONE')

    logger.info('%s : DONE', module_name)
    return


def apply_opportunities():
    from data_filter import delay_datasets, delay_table

    module_name = delay_datasets.__name__
    try:
        delay_datasets.main()
    except:
        logger.exception(module_name)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        server.sendmail(EMAIL['USER'], EMAIL['TO'], send_email % {**EMAIL, **mail_error})
        raise

    module_name = delay_table.__name__
    try:
        fname = os.path.join(os.getcwd(), '..', '..', 'data', 'data', 'delay_table.csv')
        delay_table.main(fname)
    except:
        logger.exception(module_name)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        server.sendmail(EMAIL['USER'], EMAIL['TO'], send_email % {**EMAIL, **mail_error})
        raise


def apply_estimator():
    from opportunity_estimator import add_situation2weekly_data

    dataset = ['srag', 'sragflu', 'obitoflu']
    Rscript = 'opportunity.estimator.R'

    for data in dataset:
        logger.info('Calculating estimates for dataset: %s' % data)
        module_name = 'opportunity_estimator.opportunity.estimator.R'
        try:
            run(['Rscript', '--vanilla', Rscript, '-d', 'max', '-t', data])
        except:
            logger.exception(module_name)
            mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
            server.sendmail(EMAIL['USER'], EMAIL['TO'], send_email % {**EMAIL, **mail_error})
            raise

        logger.info('Adding situation info for dataset: %s' % data)
        module_name = add_situation2weekly_data.__name__
        try:
            add_situation2weekly_data.main([data])
        except:
            logger.exception(module_name)
            mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
            server.sendmail(EMAIL['USER'], EMAIL['TO'], send_email % {**EMAIL, **mail_error})
            raise

        logger.info('... DONE')

    logger.info('opportunity_estimator : DONE')
    return


def consolidate():
    from data_filter import consolidate_datasets

    module_name = consolidate_datasets.__name__
    try:
        consolidate_datasets.main(True)
    except:
        logger.exception(module_name)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        server.sendmail(EMAIL['USER'], EMAIL['TO'], send_email % {**EMAIL, **mail_error})
        raise

    logger.info('%s : DONE', module_name)
    return


def main(flist = None, update_mem = False, module_list = None, history_files=None, dir=None, years=None):
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
    logger.info('Update modules: %s', module_list)

    if 'dbf2csv' in module_list:
        logger.info('Module dbf2csv file list: %s', flist)
        logger.info('Convert DBF to CSV')
        convert_dbf(flist)

    os.chdir('./data_filter')

    if 'email' in module_list:
        logger.info('Emails update years: %s', years)
        email_update(dir, years)

    if 'filter' in module_list:
        logger.info('Aggregate and filter data')
        apply_filters(history_files)

    if 'epiweek' in module_list:
        logger.info('Insert epiweek')
        add_epiweek()

    if 'opportunities' in module_list:
        logger.info('Create table of opportunities')
        apply_opportunities()

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
    mail_success['email_body'] = mail_error['email_body'] % {'time': time}
    server.sendmail(EMAIL['USER'], EMAIL['TO'], send_email % {**EMAIL, **mail_success})

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
    parser.add_argument('--dir', help='Base folder for e-mail update module',
                        default=None)
    parser.add_argument('--years', nargs='*', action='append', help='Base years for e-mail update module',
                        default=None)
    args = parser.parse_args()
    if args.path:
        args.path = args.path[0]
    if args.modules:
        args.modules = [x.lower() for x in args.modules[0]]
    if args.history:
        args.history = args.history[0]
    if args.years:
        args.years = args.years[0]

    main(args.path, args.mem, args.modules, args.history, args.dir, args.years)
