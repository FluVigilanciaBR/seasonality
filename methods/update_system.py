__author__ = 'Marcelo Ferreira da Costa Gomes'

import argparse
import datetime
import glob
import logging
import os
import smtplib
from argparse import RawDescriptionHelpFormatter
from email.mime.text import MIMEText
from email.mime.multipart import MIMEMultipart
from email.mime.application import MIMEApplication
from email.encoders import encode_base64
from subprocess import run
from settings import EMAIL, SERVER, REPORT
from data_filter.episem import episem, lastepiweek


logger = logging.getLogger('update_system')
logger.setLevel(logging.DEBUG)
logger_fname = 'InfoGripe_system_update.log'
fh = logging.FileHandler(logger_fname)
ch = logging.StreamHandler('InfoGripe_system_update.error.log')
rscript_path = '/usr/bin/Rscript'
ch.setLevel(logging.ERROR)
formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
fh.setFormatter(formatter)
ch.setFormatter(formatter)
logger.addHandler(fh)
logger.addHandler(ch)


mail_error = {
    'subject': "InfoGripe Updater -- error log",
    'email_body': """
    This is an automated message from InfoGripe Updater.
    System's database update started at %(time)s raised an error at module %(mdl_name)s.
    Please check the attached log for details.

    All the best,
    InfoGripe Updater Monitor. 
    """,
    **EMAIL
}
mail_success = {
    'subject': "InfoGripe Updater -- success",
    'email_body': """
    This is an automated message from InfoGripe Updater.
    System's database update started at %(time)s ran without raising any errors.

    All the best,
    InfoGripe Updater Monitor. 
    """,
    **EMAIL
}

home_path = os.path.expanduser("~")
logfile_path = os.path.join(os.getcwd(), logger_fname)
data_folder = os.path.join(os.getcwd(), '..', 'data', 'data')
public_figs_folder = os.path.join(home_path, 'codes', 'mave', 'repo', 'Boletins\ do\ InfoGripe', 'Imagens')
public_report_folder = os.path.join(home_path, 'codes', 'mave', 'repo', 'Boletins\ do\ InfoGripe')
public_report_folder_email = os.path.join(home_path, 'codes', 'mave', 'repo', 'Boletins do InfoGripe')
public_data_folder = os.path.join(home_path, 'codes', 'mave', 'repo', 'Dados', 'InfoGripe')
time = datetime.datetime.now().strftime('%Y-%m-%d %H:%M:%S')
timesmpl = datetime.datetime.now().strftime('%Y%m%d')
modules_list = ['all',
                'full_email_update',
                'local_email_update',
                'local_update',
                'email',
                'dbf2csv',
                'filter',
                'convert2mem',
                'estimator',
                'consolidate',
                'export',
                'report',
                'sendreport',
                'public_dataset']

convert_region_id = {'NI': 99,
                     'BR': 0,
                     'RegN': 1001,
                     'RegL': 1002,
                     'RegC': 1003,
                     'RegS': 1004,
                     'RegNI': 9999,
                     'N': 1,
                     'NE': 2,
                     'SE': 3,
                     'S': 4,
                     'CO': 5,
                     'RNI': 9}


def send_email(mail_dict):
    module_name = 'send_email.settings'
    logger.info(module_name)

    email_msg = MIMEMultipart()
    email_msg['From'] = '%(NAME)s <%(USER)s>' % mail_dict
    email_msg['To'] = mail_dict['TO']
    email_msg['Subject'] = mail_dict['subject']
    body = MIMEText(mail_dict['email_body'], 'plain')
    email_msg.attach(body)
    fp = open(logfile_path, encoding='utf-8')
    attachment = MIMEText(fp.read(), 'text/plain')
    attachment.add_header("Content-Disposition", "attachment", filename=logger_fname)
    email_msg.attach(attachment)
    try:
        server = smtplib.SMTP_SSL('smtp.gmail.com', 465)
        server.ehlo()
        server.login(EMAIL['USER'], EMAIL['PASSWORD'])
        server.send_message(email_msg)
        server.close()
    except Exception as exception:
        logger.exception(exception)
        raise

    return


def send_report_email(epiyear: int, epiweek: int, filtertype: str='srag', extra: str=None):
    module_name = 'send_report_email.settings'
    logger.info(module_name)

    mail_report = {
        'subject': "[InfoGripe] Boletim da SE %s %02d" % (epiyear, epiweek),
        'email_body': """
Caro(a),
      
Segue o resumo do boletim semanal gerado pelo InfoGripe, com base nos dados do Sivep-gripe até a SE %s 
%02d.
Esta é uma mensagem automática, não é necessário responder o e-mail.

Em função do tamanho do documento completo, o mesmo não será enviado por e-mail, mas pode ser feito o download do 
arquivo através do endereço usual: https://bit.ly/mave-infogripe-boletim-atual . No repositório de gráficos 
referentes às capitais (https://bit.ly/infogripe-capitais e http://bit.ly/mave-infrogripe-dados-capitais) estão 
incluídas também as análises estratificadas por grupo jurídico da unidade de notificação, permitindo avaliação de 
eventuais subnotificações, aumento no atraso de notificação/digitação, ou fluxo de atendimento distinto em cada rede.

Neste edição do resumo incluímos anexo com comparação entre as estimativas produzidas ao final da semana 53 2020 e a 
atual, para auxiliar a avaliação da perda de oportunidade de digitação em relação aos dados de dezembro.
No link http://bit.ly/infogripe-painel-se02_04-2021 disponibilizamos painel em alta resolução com os gráficos 
comparativos de todas as capitais distribuídas no formato do mapa do Brasil.

*** ATENÇÃO ***
Registros de SRAG no SIVEP-Gripe para diversos estados aparentaram uma piora significativa na oportunidade de 
digitação (tempo entre data de primeiros sintomas e data de digitação no SIVEP-Gripe dos casos notificados) refente 
aos casos ocorridos durante os últimos meses de 2020 e início de 2021. Alterações repentinas e significativas no 
perfil de atraso impactam na qualidade do estimador de casos recentes, podendo gerar subestimação de casos e, 
inclusive, falso sinal de queda. Tal efeito pode ser inferido de maneira indireta ao contrastar os registros 
inseridos à época e as taxas de ocupação de leitos. Incluímos ao final deste documento painel comparativo entre as 
estimativas geradas para as capitais ao término da semana epidemiológica 02 de 2020 e a presente atualização, 
ilustrando o impacto disso nas estimativas com base nos dados do início do ano, ainda em decorrência da mudança no 
perfil do atraso durante o mês de dezembro. Observa-se uma melhora no quadro geral, com um conjunto menor de 
capitais ainda apresentando diferenças relevantes entre os dados atuais e aqueles inseridos até a início da segunda 
quinzena de janeiro. Tal melhora pode ser identificada ao contrastar a diferença aqui apresentada e a comparação 
apresentada no boletim anterior (comparação entre as SE 2020 53 e 2021 03). Vale destacar que locais que ainda 
possuam um passivo considerável de fichas de notificação referentes ao final de 2020 podem ainda não apresentar 
diferença significativa entre essas duas atualizações, tornando necessário um acompanhamento ao longo dos primeiros 
meses de 2021 para uma avaliação adequada.
Como os dados aqui analisados se referem a notificações de hospitalizações ou óbitos, a superlotação da rede 
hospitalar, com formação de lista de espera para disponibilização de leitos, pode gerar subnotificação. Isso ocorre 
toda vez que pacientes que atendem a definição de SRAG deixam de ser notificados por não ser possível realizar a 
internação do paciente. Por causa desse risco de subnotificação, é possível que os casos de SRAG notificados na base 
SIVEP subestimem o total de casos em locais com índice de ocupação de leitos elevado. Portanto, locais com índice de 
ocupação de leitos elevado devem deixar os indicadores de SRAG em segundo plano em relação à tomada de decisão até 
que a ocupação volte a diminuir.

Sugestões de melhorias podem ser encaminhadas para o endereço eletrônico de contato, disponível no documento.

Acesse o site para mais informações:
http://info.gripe.fiocruz.br

Atenciosamente,
Equipe InfoGripe
InfoGripe - http://info.gripe.fiocruz.br
Dados abertos - http://bit.ly/mave-infogripe-dados
Boletins do InfoGripe - http://bit.ly/mave-infogripe
MAVE: Grupo de Métodos Analíticos em Vigilância Epidemiológica (PROCC/Fiocruz e EMAp/FGV), em parceria com o 
GT-Influenza da Secretaria de Vigilância em Saúde do Ministério da Saúde.
""" % (epiyear, epiweek),
        **REPORT
    }

    email_msg = MIMEMultipart()
    email_msg['From'] = '%(NAME)s <%(USER)s>' % mail_report
    email_msg['Subject'] = mail_report['subject']
    body = MIMEText(mail_report['email_body'], 'plain')
    email_msg.attach(body)

    # if filtertype == 'srag':
    #     # Base report
    #     report_fname = 'Boletim_InfoGripe_SE%s%02d.pdf' % (epiyear, epiweek)
    # elif filtertype == 'sragnofever':
    #     # Report without fever filter
    #     report_fname = 'Boletim_InfoGripe_SE%s%02d_sem_filtro_febre.pdf' % (epiyear, epiweek)
    # elif filtertype == 'hospdeath':
    #     # Report without any symptoms filter
    #     report_fname = 'Boletim_InfoGripe_SE%s%02d_sem_filtro_sintomas.pdf' % (epiyear, epiweek)

    short_version = 'Resumo_InfoGripe_%s_%02d.pdf' % (epiyear, epiweek)
    report_fname = os.path.join(public_report_folder_email, 'boletins_anteriores', short_version)

    fp = open(report_fname, 'rb')
    attachment = MIMEApplication(fp.read(), _subtype='pdf', _encoder=encode_base64)
    attachment.add_header("Content-Disposition", "attachment", filename=short_version)
    email_msg.attach(attachment)

    if extra:
        # Extra report
        report_fname = extra
        fp = open(report_fname, 'rb')
        attachment = MIMEApplication(fp.read(), _subtype='pdf', _encoder=encode_base64)
        attachment.add_header("Content-Disposition", "attachment", filename=report_fname)
        email_msg.attach(attachment)
    try:
        server = smtplib.SMTP_SSL('smtp.gmail.com', 465)
        server.ehlo()
        server.login(REPORT['USER'], REPORT['PASSWORD'])
        server.send_message(email_msg, to_addrs=mail_report['CCO'].split(', '))
        server.close()
    except Exception as exception:
        logger.exception(exception)
        raise

    logger.info('%s : DONE', module_name)
    return


def convert_dbf(flist):
    from data_filter import dbf2csv

    module_name = dbf2csv.__name__
    try:
        dbf2csv.main(flist)
    except Exception as err:
        logger.exception(module_name)
        logger.exception(err)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        send_email(mail_error)
        raise

    logger.info('%s : DONE', module_name)
    return


def email_update(dir, years, sep=','):
    from data_filter import email_extract

    module_name = email_extract.__name__

    try:
        if years:
            for year in years:
                logger.info('Updating over e-mail. Base year: %s' % year)
                email_extract.main(dir, sep=sep, year=years)
        else:
            logger.info('Updating over e-mail.')
            email_extract.main(dir, sep=sep, year=years)

    except Exception as err:
        logger.exception(module_name)
        logger.exception(err)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        send_email(mail_error)
        raise

    logger.info('%s : DONE', module_name)
    return


def apply_filters(flist=None, filtertype='srag', append_cases=None, append_delay=None):
    from data_filter import sinan_filter_of_interest

    module_name = sinan_filter_of_interest.__name__

    if not flist:
        flist = sorted(glob.glob('../data/INFLUD*.csv'))

    logger.info('Historical files: %s', flist)
    try:
        sinan_filter_of_interest.main(flist,
                                      filtertype=filtertype,
                                      append_cases=append_cases,
                                      append_delay=append_delay)
    except Exception as err:
        logger.exception(module_name)
        logger.exception(err)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        send_email(mail_error)
        raise

    logger.info('%s : DONE', module_name)

    return


def add_epiweek(filtertype='srag'):
    from data_filter import insert_epiweek

    module_name = insert_epiweek.__name__
    
    suff = ''
    if filtertype != 'srag':
        suff = '_%s' % filtertype
    
    flist = ['clean_data_srag%s.csv' % suff, 'clean_data_sragflu%s.csv' % suff, 'clean_data_obitoflu%s.csv' % suff, 
             'clean_data_sragcovid%s.csv' % suff, 'clean_data_obitocovid%s.csv' % suff, 'clean_data_obito%s.csv' % suff]
    for fname in flist:
        logger.info('Inserting epiweek on file %s' % fname)
        try:
            insert_epiweek.main(fname)
        except Exception as err:
            logger.exception(module_name)
            logger.exception(err)
            mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
            send_email(mail_error)
            raise

        logger.info('... DONE')

    logger.info('%s : DONE', module_name)
    return


def convert2mem(filtertype='srag'):
    from data_filter import sinan_convert2mem

    module_name = sinan_convert2mem.__name__

    suff = ''
    if filtertype != 'srag':
        suff = '_%s' % filtertype

    flist = ['../clean_data/clean_data_srag%s_epiweek.csv' % suff,
             '../clean_data/clean_data_sragflu%s_epiweek.csv' % suff,
             '../clean_data/clean_data_obitoflu%s_epiweek.csv' % suff,
             '../clean_data/clean_data_sragcovid%s_epiweek.csv' % suff,
             '../clean_data/clean_data_obitocovid%s_epiweek.csv' % suff,
             '../clean_data/clean_data_obito%s_epiweek.csv' % suff]
    for fname in flist:
        logger.info('Converting to MEM structure: %s' % fname)
        try:
            sinan_convert2mem.main(fname)
        except Exception as err:
            logger.exception(module_name)
            logger.exception(err)
            mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
            send_email(mail_error)
            raise

        logger.info('... DONE')

    logger.info('%s : DONE', module_name)
    return


def apply_mem(filtertype='srag'):
    from mem import sinan_mem_inset_thresholds

    module_name = sinan_mem_inset_thresholds.__name__

    suff = ''
    if filtertype != 'srag':
        suff = '_%s' % filtertype

    dataset = ['srag', 'sragflu', 'obitoflu', 'sragcovid', 'obitocovid', 'obito']
    for data in dataset:
        fname = '../clean_data/clean_data_%s%s_epiweek4mem-incidence.csv' % (data, suff)
        logger.info('Calculating MEM thresholds for dataset: %s %s' % (data, filtertype))
        try:
            sinan_mem_inset_thresholds.main(fname, out_pref='%s%s_' % (data, suff))
        except Exception as err:
            logger.exception(module_name)
            logger.exception(err)
            mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
            send_email(mail_error)
            raise

        logger.info('... DONE')

    logger.info('%s : DONE', module_name)
    return


def apply_opportunities(filtertype='srag'):
    from data_filter import delay_datasets, delay_table

    module_name = delay_datasets.__name__

    suff = ''
    if filtertype != 'srag':
        suff = '_%s' % filtertype

    try:
        delay_datasets.main(filtertype)
    except Exception as err:
        logger.exception(module_name)
        logger.exception(err)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        send_email(mail_error)
        raise

    module_name = delay_table.__name__
    try:
        fname = os.path.join(data_folder, 'delay_table%s.csv' % suff)
        delay_table.main(fname, filtertype)
    except Exception as err:
        logger.exception(module_name)
        logger.exception(err)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        send_email(mail_error)
        raise


def apply_estimator(date='max', filtertype='srag'):
    from opportunity_estimator import add_situation2weekly_data

    dataset = ['srag', 'sragflu', 'obitoflu', 'obito', 'sragcovid', 'obitocovid']
    Rscript = 'opportunity.estimator.R'

    for data in dataset:
        logger.info('Calculating estimates for dataset: %s' % data)
        module_name = 'opportunity_estimator.opportunity.estimator.R'
        try:
            run([rscript_path, '--vanilla', Rscript, '-d', date, '-t', data, '-f', filtertype], check=True)
        except Exception as err:
            logger.exception(module_name)
            logger.exception(err)
            mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
            send_email(mail_error)
            raise

        logger.info('Adding situation info for dataset: %s' % data)
        try:
            add_situation2weekly_data.main([data], filtertype)
        except Exception as err:
            logger.exception(module_name)
            logger.exception(err)
            mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
            send_email(mail_error)
            raise

        logger.info('... DONE')

    if filtertype == 'sragnofever':
        data = 'srag'
        logger.info('Calculating estimates for capitals and macroregions of health: %s' % data)
        module_name = 'opportunity_estimator.nowcastingCapitaisMacrosaude.R'
        try:
            cwd = os.getcwd()
            os.chdir('../nowcasting_capitais')
            Rscript = 'nowcastingCapitaisMacrosaude.R'
            run([rscript_path, '--vanilla', Rscript, '-d', date, '-t', data, '-f', filtertype, '--graphs'], check=True)
            os.chdir(cwd)
        except Exception as err:
            logger.exception(module_name)
            logger.exception(err)
            mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
            send_email(mail_error)
            raise
        logger.info('... DONE')

    logger.info('opportunity_estimator : DONE')
    return


def consolidate(fname=None, filtertype='srag'):
    from data_filter import consolidate_datasets
    from data_filter.settings import DATABASE

    module_name = consolidate_datasets.__name__

    try:
        consolidate_datasets.main(True, filtertype)
    except Exception as err:
        logger.exception(module_name)
        logger.exception(err)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        send_email(mail_error)
        raise
    logger.info('%s : DONE', module_name)

    module_name = 'consolidate.pg_dump'
    try:
        if not fname:
            fname = os.path.join(data_folder, 'infogripe%s.dump' % timesmpl)
        run(['pg_dump', '-Fc', '--host=%(HOST)s' % DATABASE, '--username=%(USER)s' % DATABASE,
             '--dbname=%(NAME)s' % DATABASE, '-w', '--file', fname], check=True)
    except Exception as err:
        logger.exception(module_name)
        logger.exception(err)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        send_email(mail_error)
        raise

    logger.info('%s : DONE', module_name)
    return


def exportdb(fname=None):
    from time import sleep
    module_name = 'consolidate.export'
    tries = 10
    for i in range(tries):
        try:
            if not fname:
                fname = os.path.join(data_folder, 'infogripe%s.dump' % timesmpl)

            run(['scp', '-C', fname, '%(USER)s@%(HOST)s:~/update/infogripe.dump' % SERVER], check=True)
        except Exception as err:
            if i < tries - 1:  # i is zero indexed
                sleep(2)
                continue
            else:
                logger.exception(module_name)
                logger.exception(err)
                mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
                send_email(mail_error)
                raise
        break

    logger.info('%s : DONE', module_name)
    return


def generate_report(epiyear=None, epiweek=None, plot=None, filtertype='srag'):

    Rscript = 'report.data.R'

    logger.info('Generating weekly report: %s %s' % (epiyear, epiweek))
    module_name = 'report.report.data.R'
    try:
        if plot:
            run([rscript_path, '--vanilla', Rscript, '-y', str(epiyear), '-w', str(epiweek), '-p', '-f', filtertype],
                check=True)
        else:
            run([rscript_path, '--vanilla', Rscript, '-y', str(epiyear), '-w', str(epiweek), '-f', filtertype], check=True)
    except Exception as err:
        logger.exception(module_name)
        logger.exception(err)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        send_email(mail_error)
        raise

    logger.info('%s : DONE', module_name)
    return


def generate_public_datasets(filtertype='srag'):

    import pandas as pd

    module_name = 'generate_public_datasets'

    suff = ''
    if filtertype != 'srag':
        suff = '_%s' % filtertype

    try:
        situation_dict = {
            'stable': 'Dado estável. Sujeito a pequenas alterações.',
            'estimated': 'Estimado. Sujeito a alterações.',
            'unknown': 'Dados incompletos. Sujeito a grandes alterações.',
            'incomplete': 'Dados incompletos. Sujeito a grandes alterações.'
        }

        dataset_id_map = {
            1: 'srag',
            2: 'sragflu',
            3: 'obitoflu',
            4: 'sragcovid',
            5: 'obitocovid',
            6: 'obito'
        }

        alert_dict = {
            1: 'valor baixo',
            2: 'valor epidêmico',
            3: 'valor alto',
            4: 'valor muito alto'
        }

        rename_cols = {
            'Situation': 'Situação do dado',
            'SRAG': 'Casos semanais reportados até a última atualização',
            'POSITIVE_CASES': "Testes positivos",
            'FLU_A': "Influenza A",
            'FLU_B': "Influenza B",
            "SARS2": "SARS-CoV-2",
            "VSR": 'Vírus sincicial respiratório (VSR)',
            "ADNO": "Adenovirus",
            "RINO": "Rinovirus",
            "BOCA": "Bocavirus",
            "METAP": "Metapneumovirus",
            "PARA1": "Parainfluenza 1",
            "PARA2": "Parainfluenza 2",
            "PARA3": "Parainfluenza 3",
            "PARA4": "Parainfluenza 4",
            "OTHERS": "Outros virus",
            'NEGATIVE': "Testes negativos",
            'NOTTESTED': "Casos sem teste laboratorial",
            'DELAYED': "Casos aguardando resultado",
            'TESTING_IGNORED': "Casos sem informação laboratorial",
            'INCONCLUSIVE': 'Resultado inconclusivo',
            'epiyear': 'Ano epidemiológico',
            'epiweek': 'Semana epidemiológica',
            'epiyearweek': 'Ano e semana epidemiológica',
            'cntry_percentage': 'Percentual em relação ao país',
            'bounded_97.5%': 'limite superior da estimativa',
            '2.5%': 'limite inferior da estimativa',
            '50%': 'casos estimados',
            'rolling_average': 'média móvel',
            'alert': 'nível semanal',
            'Run date': 'data de publicação',
            'População': 'População de referência para cálculo de incidência',
            'temporadas utilizadas para os corredores endêmicos':
                'temporadas consideradas regulares',
            'population': 'População'
        }

        suff_out = {
            'srag': '',
            'sragnofever': '_sem_filtro_febre',
            'hospdeath': '_sem_filtro_sintomas'
        }

        #MEM
        fname = os.path.join(data_folder, 'mem-report%s.csv' % suff)
        dfreport = pd.read_csv(fname)
        dfreport = dfreport.rename(columns={'População': 'População de referência para cálculo de incidência',
                                            'temporadas utilizadas para os corredores endêmicos': 'temporadas consideradas regulares'})

        tgt_cols = ['UF', 'Unidade da Federação', 'Tipo', 'dado', 'escala',
                    'limiar pré-epidêmico',  'intensidade alta', 'intensidade muito alta',
                    'temporadas consideradas regulares',
                    'População de referência para cálculo de incidência']
        dfreport = dfreport[tgt_cols]

        fname = os.path.join(data_folder, 'mem-typical%s.csv' % suff)
        dftypical = pd.read_csv(fname)
        dftypical = dftypical.drop(columns='População').merge(dfreport)
        dftypical.loc[dftypical.Tipo != 'Estado', 'UF'] = dftypical.loc[dftypical.Tipo != 'Estado', 'UF'].map(
            convert_region_id)

        fname = os.path.join(data_folder, 'valores_esperados_por_localidade%s.csv' % suff_out[filtertype])
        tgt_cols = ['UF', 'Unidade da Federação', 'Tipo', 'dado', 'escala', 'epiweek',
                    'corredor baixo', 'corredor mediano', 'corredor alto',
                    'limiar pré-epidêmico', 'intensidade alta',
                    'intensidade muito alta', 'temporadas consideradas regulares',
                    'População de referência para cálculo de incidência']
        dftypical[tgt_cols].rename(columns=rename_cols).to_csv(fname, sep=';', index=False, decimal=',')
        fname = os.path.join(public_data_folder, 'valores_esperados_por_localidade%s.csv' % suff_out[filtertype])
        dftypical[tgt_cols].rename(columns=rename_cols).to_csv(fname, sep=';', index=False, decimal=',')

        #time series
        fname = os.path.join(data_folder, 'current_estimated_values%s.csv' % suff)
        df = pd.read_csv(fname)
        run_date = df['Run date'].unique()[0]
        tgt_cols = ['Run date', 'UF', 'dado', 'escala', 'epiyear', 'epiweek', 'Situation', 'SRAG', '2.5%', '50%',
                    'bounded_97.5%', 'cntry_percentage', 'rolling_average', 'population']
        df = df[tgt_cols].copy()
        df = df.merge(dfreport[['UF',
                                'Unidade da Federação',
                                'Tipo',
                                'dado',
                                'escala',
                                'limiar pré-epidêmico',
                                'intensidade alta',
                                'intensidade muito alta']], how='left')
        df = df[['Run date',
                 'UF',
                 'Unidade da Federação',
                 'Tipo',
                 'dado',
                 'escala',
                 'epiyear',
                 'epiweek',
                 'Situation',
                 'SRAG',
                 '2.5%',
                 '50%',
                 'rolling_average',
                 'bounded_97.5%',
                 'cntry_percentage',
                 'population',
                 'limiar pré-epidêmico',
                 'intensidade alta',
                 'intensidade muito alta']]
        df.loc[df.Situation != 'estimated', ['2.5%', '50%', 'bounded_97.5%']] = None
        epiweekmax = df.loc[df.Situation == 'estimated',
                            ['epiyear', 'epiweek']].sort_values(by=['epiyear', 'epiweek']).tail(1)
        df.loc[(df.epiyear == epiweekmax.epiyear.values[0]) &
               (df.epiweek >= epiweekmax.epiweek.values[0]-1),
               'SRAG'] = None
        if epiweekmax.epiweek.values[0] == 1:
            epiyearprev = epiweekmax.epiyear.values[0] - 1
            epiwkmax = df.epiweek[df.Situation == 'estimated'].max()
            df.loc[(df.epiyear == epiyearprev) &
                   (df.epiweek == epiwkmax),
                   'SRAG'] = None

        df.Situation = df.Situation.map(situation_dict)
        df.loc[df.Tipo != 'Estado', 'UF'] = df.loc[df.Tipo != 'Estado', 'UF'].map(convert_region_id)
        df.UF = df.UF.astype(int)

        ## Add alert levels
        fname = os.path.join(data_folder, 'weekly_alert%s.csv' % suff)
        dfalert = pd.read_csv(fname)[['dataset_id', 'territory_id', 'epiyear', 'epiweek', 'alert']]
        dfalert.dataset_id = dfalert.dataset_id.map(dataset_id_map)
        dfalert.alert = dfalert.alert.map(alert_dict)
        df = df.merge(dfalert,
                      left_on=['UF', 'dado', 'epiyear', 'epiweek'],
                      right_on=['territory_id', 'dataset_id', 'epiyear', 'epiweek'], how='left')
        df = df.drop(columns={'territory_id', 'dataset_id'})

        def alert_level_rolling(x):
            if pd.isnull(x[0]):
                return None
            if x[0] >= x[2]:
                return 'Vermelho'
            if x[0] >= x[1]:
                return 'Amarelo'
            return 'Verde'

        df['nível por média móvel'] = df[['rolling_average',
                                          'limiar pré-epidêmico',
                                          'intensidade alta']].apply(axis=1, func=alert_level_rolling)
        df = df.rename(columns=rename_cols)
        df.dado = pd.Categorical(df.dado, ['srag', 'sragflu', 'sragcovid', 'obito', 'obitoflu', 'obitocovid'])
        df = df.sort_values(by=['Ano epidemiológico', 'escala', 'dado', 'UF', 'Semana epidemiológica']).reset_index(
            drop=True)
        fname = os.path.join(data_folder, 'serie_temporal_com_estimativas_recentes%s.csv' % suff_out[filtertype])
        df.to_csv(fname, sep=';', index=False, decimal=',')
        fname = os.path.join(public_data_folder, 'serie_temporal_com_estimativas_recentes%s.csv' % suff_out[filtertype])
        df.to_csv(fname, sep=';', index=False, decimal=',')

        # tabela:
        if epiweekmax.epiweek.values[0] == 1:
            df = df.loc[(((df['Ano epidemiológico'] == epiyearprev) & (df['Semana epidemiológica'] == epiwkmax)) |
                         ((df['Ano epidemiológico'] == epiyearprev+1) & (df['Semana epidemiológica'] == 1))) &
                        (df.dado == 'srag') &
                        (df.escala == 'incidência'),
                        ['UF', 'Unidade da Federação', 'dado', 'escala', 'Ano epidemiológico', 'Semana epidemiológica',
                         'casos estimados', 'média móvel', 'nível semanal', 'nível por média móvel']].copy()
        else:
            df = df.loc[(df['Ano epidemiológico'] == epiweekmax.epiyear.values[0]) &
                        (df['Semana epidemiológica'].isin([epiweekmax.epiweek-1, epiweekmax.epiweek])) &
                        (df.dado == 'srag') &
                        (df.escala == 'incidência'),
                        ['UF', 'Unidade da Federação', 'dado', 'escala', 'Ano epidemiológico', 'Semana epidemiológica',
                         'casos estimados', 'média móvel', 'nível semanal', 'nível por média móvel']].copy()

        fname = os.path.join(data_folder, 'tabela_de_alerta%s.csv' % suff_out[filtertype])
        df.to_csv(fname, sep=';', index=False, decimal=',')
        fname = os.path.join(public_data_folder, 'tabela_de_alerta%s.csv' % suff_out[filtertype])
        df.to_csv(fname, sep=';', index=False, decimal=',')

        # Data by age, gender, and virus:
        fname = os.path.join(data_folder, 'clean_data_epiweek-weekly-incidence_w_situation%s.csv' % suff)
        df = pd.read_csv(fname)
        df.Situation = df.Situation.map(situation_dict)
        df['Run date'] = run_date
        df = df.rename(columns=rename_cols)
        df.loc[df.Tipo != 'Estado', 'UF'] = df.loc[df.Tipo != 'Estado', 'UF'].map(convert_region_id)

        tgt_cols = ['data de publicação',
                    'UF',
                    'Unidade da Federação',
                    'Tipo',
                    'dado',
                    'escala',
                    'sexo',
                    'Ano epidemiológico',
                    'Semana epidemiológica',
                    'Ano e semana epidemiológica',
                    'Situação do dado',
                    'Casos semanais reportados até a última atualização',
                    'Idade desconhecida',
                    '< 2 anos', '0-4 anos', '10-19 anos', '2-4 anos', '20-29 anos', '30-39 anos', '40-49 anos',
                    '5-9 anos', '50-59 anos', '60+ anos',
                    'Testes positivos', 'Testes negativos', 'Casos aguardando resultado',
                    'Casos sem informação laboratorial', 'Casos sem teste laboratorial',
                    'Resultado inconclusivo',
                    'Influenza A', 'Influenza B', 'SARS-CoV-2', 'Vírus sincicial respiratório (VSR)',
                    'Parainfluenza 1', 'Parainfluenza 2', 'Parainfluenza 3', 'Parainfluenza 4', 'Adenovirus',
                    'Rinovirus', 'Bocavirus', 'Metapneumovirus', 'Outros virus'
                    ]
        fname = os.path.join(data_folder, 'dados_semanais_faixa_etaria_sexo_virus%s.csv' % suff_out[filtertype])
        df[tgt_cols].to_csv(fname, sep=';', index=False, decimal=',')
        fname = os.path.join(public_data_folder, 'dados_semanais_faixa_etaria_sexo_virus%s.csv' % suff_out[filtertype])
        df[tgt_cols].to_csv(fname, sep=';', index=False, decimal=',')

        run(['cp --force ./report/Figs/Territory_*_dataset_1_timeseries%s.png %s/.' % (suff, public_figs_folder)],
            check=True, shell=True)
        run(['rename "s/Territory/Territorio/" %s/Territory_*png' % public_figs_folder], check=True, shell=True)
        run(['rename -f "s/dataset_1/SRAG/" %s/Territorio_*png' % public_figs_folder], check=True, shell=True)
        run(['rename -f "s/timeseries/serietemporal/" %s/Territorio_*png' % public_figs_folder], check=True, shell=True)
        run(['rename -f "s/%s/%s/" %s/Territorio_*png' % (suff, suff_out[filtertype], public_figs_folder)], check=True,
            shell=True)
        run(['rm --force %s/Territory_*png' % public_figs_folder],
            check=True, shell=True)
        run(['cp --force ./report/Boletim_InfoGripe_atual%s.pdf %s/.' % (suff_out[filtertype], public_report_folder)],
            check=True, shell=True)

        if filtertype == 'sragnofever':
            run(['cp --force ./nowcasting_capitais/Figs/Capitais/fig*.png %s/Capitais/.' % public_figs_folder],
                check=True, shell=True)
            run(['cp --force ./nowcasting_capitais/Figs/Capitais/Mapa_capitais_tendencia.png %s/Capitais/.' %
                 public_figs_folder],
                check=True, shell=True)
            run(['cp --force ./nowcasting_capitais/capitais_serie_estimativas_tendencia_sem_filtro_febre.csv %s/.' %
                 public_data_folder],
                check=True, shell=True)
            run(['cp --force ./nowcasting_capitais/Figs/MACSAUD/fig*.png %s/Macrorregioes_de_saude/.' % public_figs_folder],
                check=True, shell=True)
            run(['cp --force ./nowcasting_capitais/Figs/MACSAUD/Mapa*tendencia.png %s/Macrorregioes_de_saude/.' %
                 public_figs_folder],
                check=True, shell=True)
            run(['cp --force ./nowcasting_capitais/macsaud_serie_estimativas_tendencia_sem_filtro_febre.csv %s/.' %
                 public_data_folder],
                check=True, shell=True)

    except Exception as err:
        logger.exception(module_name)
        logger.exception(err)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': module_name}
        send_email(mail_error)
        raise

    logger.info('%s : DONE', module_name)

    return


def main(flist=None, update_mem=False, module_list=None, history_files=None, dir=None, sep=',', years=None, date='max',
         dbdump=None, plot=None, filtertype='srag', extra=None, append_cases=None, append_delay=None):
    """
    Run all scripts to update the system with new database.

    :param flist:
    :param update_mem:
    :param module_list:
    :param history_files:
    :param dir:
    :param sep:
    :param years:
    :param date:
    :param dbdump:
    :param plot:
    :param filtertype:
    :param extra:
    """

    logger.info('System update: START')
    for m in module_list:
        if m not in modules_list:
            logger.error('Unknown module request: %s', m)
            mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': 'caller'}
            send_email(mail_error)
            exit(0)

    if filtertype not in ['srag', 'sragnofever', 'hospdeath']:
        logger.error('Unknown filter type: %s\nAccepted values: srag, sragnofever, hospdeath', filtertype)
        mail_error['email_body'] = mail_error['email_body'] % {'time': time, 'mdl_name': 'caller'}
        send_email(mail_error)
        exit(0)

    if module_list and 'all' in module_list:
        module_list = ['dbf2csv',
                       'filter',
                       'convert2mem',
                       'estimator',
                       'consolidate',
                       'public_dataset',
                       'export',
                       'report',
                       'sendreport']
    if module_list and 'full_email_update' in module_list:
        module_list = ['email',
                       'filter',
                       'convert2mem',
                       'estimator',
                       'consolidate',
                       'public_dataset',
                       'export',
                       'report',
                       'sendreport']

    if module_list and 'local_email_update' in module_list:
        module_list = ['email',
                       'filter',
                       'convert2mem',
                       'estimator',
                       'consolidate',
                       'public_dataset',
                       'report']

    if module_list and 'local_update' in module_list:
        module_list = ['convert2mem',
                       'estimator',
                       'consolidate',
                       'public_dataset',
                       'report']

    logger.info('Update MEM: %s', update_mem)
    logger.info('Update modules: %s', module_list)
    logger.info('Parameters: %s' % {'flist': flist,
                                    'update_mem': update_mem,
                                    'module_list': module_list,
                                    'history_files': history_files,
                                    'dir': dir,
                                    'sep': sep,
                                    'years': years,
                                    'date': date,
                                    'dbdump': dbdump,
                                    'plot': plot,
                                    'filtertype': filtertype,
                                    'extra': extra})

    if 'dbf2csv' in module_list:
        logger.info('Module dbf2csv file list: %s', flist)
        logger.info('Convert DBF to CSV')
        convert_dbf(flist)

    os.chdir('./data_filter')

    if 'email' in module_list:
        logger.info('Emails update years: %s', years)
        email_update(dir, years, sep=sep)

    if 'filter' in module_list:
        logger.info('Aggregate and filter data')
        apply_filters(history_files, filtertype, append_cases=append_cases, append_delay=append_delay)

    if 'epiweek' in module_list:
        logger.info('Insert epiweek')
        add_epiweek(filtertype)

    if 'opportunities' in module_list:
        logger.info('Create table of opportunities')
        apply_opportunities(filtertype)

    if 'convert2mem' in module_list:
        logger.info('Convert to MEM structure and aggregate by epiweek')
        convert2mem(filtertype)

    os.chdir('../mem')
    if update_mem:
        logger.info('Apply MEM')
        apply_mem(filtertype)

    os.chdir('../opportunity_estimator')
    if 'estimator' in module_list:
        logger.info('Apply opportunity estimator')
        apply_estimator(date, filtertype)

    os.chdir('../data_filter')
    if 'consolidate' in module_list:
        logger.info('Consolidate dataset and update DB')
        consolidate(dbdump, filtertype)

    os.chdir('../')
    os.chdir('./report')
    epiyear, epiweek = episem(date).split('W')
    if int(epiweek) == 1:
        epiyear = int(epiyear) - 1
        epiweek = int(lastepiweek(epiyear))
    else:
        epiyear = int(epiyear)
        epiweek = int(epiweek) - 1

    if 'report' in module_list:
        logger.info('Report generation')
        generate_report(epiyear=epiyear, epiweek=epiweek, plot=plot, filtertype=filtertype)

    if 'sendreport' in module_list:
        logger.info('Send report over email')
        send_report_email(epiyear=epiyear, epiweek=epiweek, filtertype=filtertype, extra=extra)

    os.chdir('../')

    if 'public_dataset' in module_list:
        logger.info('Generate public dataset')
        generate_public_datasets(filtertype)

    if 'export' in module_list:
        logger.info('Export DB')
        exportdb(dbdump)

    logger.info('System update: DONE')
    mail_success['email_body'] = mail_success['email_body'] % {'time': time}
    send_email(mail_success)


if __name__ == '__main__':
    today = datetime.datetime.strftime(datetime.datetime.today(), '%Y-%m-%d')

    parser = argparse.ArgumentParser(description="Update InfoGripe database.\n" +
                                                 "python3 update_system.py --mem --path ./data/influ*.DBF\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--mem', action='store_true', help='Update MEM thresholds.')
    parser.add_argument('--modules', nargs='*', action='append',
                        help='Which modules should be ran.\nModule list: %s' % modules_list,
                        default=[])
    parser.add_argument('--path', nargs='*', action='append', help='Path to data file. Optional',
                        default=None)
    parser.add_argument('--history', nargs='*', action='append', help='Path to historical notifications csv files. '
                                                                      'Optional',
                        default=None)
    parser.add_argument('--dir', help='Base folder for e-mail update module. Optional',
                        default=None)
    parser.add_argument('--sep', help='Column separator for csv files. Optional',
                        default=',')
    parser.add_argument('--years', nargs='*', action='append', help='Base years for e-mail update module. Optional',
                        default=None)
    parser.add_argument('--date', help='Base date for estimator in the format YYYY-MM-DD or max. Optional.',
                        default=today)
    parser.add_argument('--dbdump', help='Path do database dump for export. Optional', default=None)
    parser.add_argument('--plot', action='store_true', help='Should the module updat report plots?')
    parser.add_argument('--filtertype', help='Default=srag. Which filter should be used? [srag, sragnofever, '
                                             'hospdeath]', default='srag')
    parser.add_argument('--extra', help='Default=None. Path to additional pdf for mailing list (if any)', default=None)
    parser.add_argument('--append_cases', help='Default=None. Path to case files to append to',
                        default=None)
    parser.add_argument('--append_delay', help='Default=None. Path to delay file to append to',
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

    main(flist=args.path, update_mem=args.mem, module_list=args.modules, history_files=args.history, dir=args.dir,
         sep=args.sep, years=args.years, date=args.date, dbdump=args.dbdump, plot=args.plot, filtertype=args.filtertype,
         extra=args.extra, append_cases=args.append_cases, append_delay=args.append_delay)
