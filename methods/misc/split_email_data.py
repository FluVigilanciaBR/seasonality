# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import glob
import os
import argparse
import logging
import re
import pandas as pd
from argparse import RawDescriptionHelpFormatter
from datetime import date
from dbfread import DBF
from fnmatch import fnmatch
from patoolib import extract_archive
from subprocess import run

module_logger = logging.getLogger('update_system.email_extract')
dict_dtypes = {
    'NU_NOTIFIC': 'str',
    'DT_NOTIFIC': 'str',
    'SEM_NOT': 'Int64',
    'ANO': 'Int16',
    'DT_SIN_PRI': 'str',
    'SEM_PRI': 'Int64',
    'SG_UF_NOT': 'str',
    'ID_REGIONA': 'str',
    'CO_REGIONA': 'Int32',
    'ID_MUNICIP': 'str',
    'CO_MUN_NOT': 'Int32',
    'ID_UNIDADE': 'str',
    'CO_UNI_NOT': 'Int32',
    'NM_UN_INTE': 'str',
    'CO_UN_INTE': 'Int32',
    'LAB_AN': 'str',
    'CO_LAB_AN': 'Int32',
    'RES_CAPITAL': 'str',
    'NOT_CAPITAL': 'str',
    'NU_CPF': 'Int64',
    'NU_CNS': 'str',
    'NM_PACIENT': 'str',
    'CS_SEXO': 'str',
    'DT_NASC': 'str',
    'NU_IDADE_N': 'str',
    'TP_IDADE': 'Int8',
    'COD_IDADE': 'str',
    'CS_GESTANT': 'Int8',
    'CS_RACA': 'Int8',
    'CS_ETINIA': 'str',
    'TP_POV_CT': 'str',
    'CS_ESCOL_N': 'Int8',
    'NM_MAE_PAC': 'str',
    'NU_CEP': 'str',
    'ID_PAIS': 'str',
    'CO_PAIS': 'Int16',
    'SG_UF': 'str',
    'ID_RG_RESI': 'str',
    'CO_RG_RESI': 'Int16',
    'ID_MN_RESI': 'str',
    'CO_MUN_RES': 'Int32',
    'NM_BAIRRO': 'str',
    'NM_LOGRADO': 'str',
    'NU_NUMERO': 'str',
    'NM_COMPLEM': 'str',
    'NU_DDD_TEL': 'Int8',
    'NU_TELEFON': 'str',
    'CS_ZONA': 'Int8',
    'SURTO_SG': 'Int8',
    'NOSOCOMIAL': 'Int8',
    'AVE_SUINO': 'Int8',
    'FEBRE': 'Int8',
    'TOSSE': 'Int8',
    'GARGANTA': 'Int8',
    'DISPNEIA': 'Int8',
    'DESC_RESP': 'Int8',
    'SATURACAO': 'Int8',
    'DIARREIA': 'Int8',
    'VOMITO': 'Int8',
    'OUTRO_SIN': 'Int8',
    'OUTRO_DES': 'str',
    'PUERPERA': 'Int8',
    'FATOR_RISC': 'str',
    'CARDIOPATI': 'Int8',
    'HEMATOLOGI': 'Int8',
    'SIND_DOWN': 'Int8',
    'HEPATICA': 'Int8',
    'ASMA': 'Int8',
    'DIABETES': 'Int8',
    'NEUROLOGIC': 'Int8',
    'PNEUMOPATI': 'Int8',
    'IMUNODEPRE': 'Int8',
    'RENAL': 'Int8',
    'OBESIDADE': 'Int8',
    'OBES_IMC': 'str',
    'OUT_MORBI': 'Int8',
    'MORB_DESC': 'str',
    'VACINA': 'Int8',
    'DT_UT_DOSE': 'str',
    'MAE_VAC': 'Int8',
    'DT_VAC_MAE': 'str',
    'M_AMAMENTA': 'Int8',
    'DT_DOSEUNI': 'str',
    'DT_1_DOSE': 'str',
    'DT_2_DOSE': 'str',
    'ANTIVIRAL': 'Int8',
    'TP_ANTIVIR': 'Int8',
    'OUT_ANTIV': 'str',
    'DT_ANTIVIR': 'str',
    'HOSPITAL': 'Int8',
    'DT_INTERNA': 'str',
    'SG_UF_INTE': 'str',
    'ID_RG_INTE': 'str',
    'CO_RG_INTE': 'Int16',
    'ID_MN_INTE': 'str',
    'CO_MU_INTE': 'Int32',
    'UTI': 'Int8',
    'DT_ENTUTI': 'str',
    'DT_SAIDUTI': 'str',
    'SUPORT_VEN': 'Int8',
    'RAIOX_RES': 'Int8',
    'RAIOX_OUT': 'str',
    'DT_RAIOX': 'str',
    'AMOSTRA': 'Int8',
    'DT_COLETA': 'str',
    'TP_AMOSTRA': 'Int8',
    'OUT_AMOST': 'str',
    'REQUI_GAL': 'Int64',
    'IF_RESUL': 'Int8',
    'DT_IF': 'str',
    'POS_IF_FLU': 'Int8',
    'TP_FLU_IF': 'Int8',
    'POS_IF_OUT': 'Int8',
    'IF_VSR': 'Int8',
    'IF_PARA1': 'Int8',
    'IF_PARA2': 'Int8',
    'IF_PARA3': 'Int8',
    'IF_ADENO': 'Int8',
    'IF_OUTRO': 'Int8',
    'DS_IF_OUT': 'object',
    'LAB_IF': 'str',
    'CO_LAB_IF': 'Int64',
    'PCR_RESUL': 'Int8',
    'DT_PCR': 'str',
    'POS_PCRFLU': 'Int8',
    'TP_FLU_PCR': 'Int8',
    'PCR_FLUASU': 'Int8',
    'FLUASU_OUT': 'str',
    'PCR_FLUBLI': 'Int8',
    'FLUBLI_OUT': 'str',
    'POS_PCROUT': 'Int8',
    'PCR_VSR': 'Int8',
    'PCR_PARA1': 'Int8',
    'PCR_PARA2': 'Int8',
    'PCR_PARA3': 'Int8',
    'PCR_PARA4': 'Int8',
    'PCR_ADENO': 'Int8',
    'PCR_METAP': 'Int8',
    'PCR_BOCA': 'Int8',
    'PCR_RINO': 'Int8',
    'PCR_OUTRO': 'Int8',
    'DS_PCR_OUT': 'str',
    'LAB_PCR': 'str',
    'CO_LAB_PCR': 'Int64',
    'CLASSI_FIN': 'Int8',
    'CLASSI_OUT': 'str',
    'CRITERIO': 'Int8',
    'EVOLUCAO': 'Int8',
    'DT_EVOLUCA': 'str',
    'NU_DO': 'Int64',
    'DT_ENCERRA': 'str',
    'OBSERVA': 'str',
    'NOME_PROF': 'str',
    'REG_PROF': 'str',
    'DT_DIGITA': 'str',
    'HISTO_VGM': 'Int8',
    'PAIS_VGM': 'str',
    'CO_PS_VGM': 'Int16',
    'LO_PS_VGM': 'str',
    'DT_VGM': 'str',
    'DT_RT_VGM': 'str',
    'PCR_SARS2': 'Int8',
    'PAC_COCBO': 'str',
    'PAC_DSCBO': 'str',
    'OUT_ANIM': 'str',
    'DOR_ABD': 'Int8',
    'FADIGA': 'Int8',
    'PERD_OLFT': 'Int8',
    'PERD_PALA': 'Int8',
    'TOMO_RES': 'Int8',
    'TOMO_OUT': 'str',
    'DT_TOMO': 'str',
    'TP_TES_AN': 'Int8',
    'DT_RES_AN': 'str',
    'RES_AN': 'Int8',
    'POS_AN_FLU': 'Int8',
    'TP_FLU_AN': 'Int8',
    'POS_AN_OUT': 'Int8',
    'AN_SARS2': 'Int8',
    'AN_VSR': 'Int8',
    'AN_PARA1': 'Int8',
    'AN_PARA2': 'Int8',
    'AN_PARA3': 'Int8',
    'AN_ADENO': 'Int8',
    'AN_OUTRO': 'Int8',
    'DS_AN_OUT': 'str',
    'TP_AM_SOR': 'Int8',
    'SOR_OUT': 'str',
    'DT_CO_SOR': 'str',
    'TP_SOR': 'Int8',
    'OUT_SOR': 'str',
    'DT_RES': 'str',
    'RES_IGG': 'Int8',
    'RES_IGM': 'Int8',
    'RES_IGA': 'Int8',
    'DT_ATUALIZACAO': 'str',
    'DOSE_1_COV': 'str',
    'DOSE_2_COV': 'str',
    'DOSE_REF': 'str',
    'LOTE_1_COV': 'str',
    'LOTE_2_COV': 'str',
    'FAB_COV': 'str',
    'FAB_COV1': 'str',
    'FAB_COV2': 'str',
    'FAB_COVREF': 'str',
    'VACINA_COV': 'Int8',
    '@VERSION': 'Int64',
    '@TIMESTAMP': 'str'
}


def remove_whitespace(x):
    if isinstance(x, object):
        x = x.str.lstrip().str.rstrip()
        x = x.where(lambda v: v != '', None)
        return x
    else:
        return x


def date_cleanup(df, dt_cols):
    '''
    Standardize column data and discard those without notification and/or first symptoms date.
    :param df: notifications data frame
    :param dt_cols: list of data columns
    :return: df: standardized data frame
    '''
    module_logger.info('Date columns cleanup: %s' % dt_cols)

    # Filter by notification date
    df = df.where(df != -1, None)
    df[dt_cols] = df[dt_cols].where(df[dt_cols] != 10101, None)

    # Convert all date related columns to datetime format
    for col in dt_cols:
        # Convert all date columns to datetime format. Output will have the format YYYY-MM-DD
        dtformat = '%Y%m%d'
        sample = df.loc[~pd.isnull(df[col]), col].values[0]
        if isinstance(sample, str):
            df[col] = remove_whitespace(df[col])

        if sum(~pd.isnull(df[col])) > 0:
            sample = df.loc[~pd.isnull(df[col]), col].values[0]
            if isinstance(sample, str):
                if 'T' in sample or any(df.loc[~pd.isnull(df[col]), col].str.contains('T')):
                    df[col] = pd.to_datetime(df[col].str[:10], errors='coerce', utc=True).dt.date
                else:
                    dtsep = '-'
                    if '/' in sample:
                        dtsep = '/'
                    dttest = pd.DataFrame(list(
                        df.loc[~pd.isnull(df[col]), col].str.split(dtsep)
                    ))
                    maxvals = [int(dttest[i].max()) for i in range(3)]
                    del dttest
                    yearpos = maxvals.index(max(maxvals))
                    if yearpos == 2:
                        if maxvals[1] > 12:
                            dtformat = '%m' + dtsep + '%d' + dtsep + '%Y'
                        else:
                            dtformat = '%d' + dtsep + '%m' + dtsep + '%Y'
                    else:
                        dtformat = '%Y' + dtsep + '%m' + dtsep + '%d'
                    df[col] = pd.to_datetime(df[col], errors='coerce', format=dtformat)
            else:
                df[col] = pd.to_datetime(df[col], errors='coerce', format=dtformat)
        else:
            df[col] = pd.to_datetime(df[col])

    return df


def write_to_folder(df, year, dir=None, append=False):

    if not dir:
        dir = os.getcwd()

    output = os.path.join(dir, '..', 'data', 'INFLUD%s.csv' % year)
    if append:
        df = pd.concat([df, pd.read_csv(output, dtype=df.dtypes.to_dict())], ignore_index=True, sort=False)
    df.columns = df.columns.str.upper()
    df.to_csv(output, index=False, encoding='utf-8', date_format='%Y-%m-%d')
    output = os.path.join(os.path.expanduser('~'),
                          'ownCloud',
                          'Fiocruz',
                          'Influenza',
                          'Dados-equipe',
                          'dados-brutos',
                          'INFLUD%s.zip' % year)
    df.to_csv(output, compression={'method': 'zip', 'archive_name': 'INFLUD%s.csv' % year})

    return


def extract_csv(dir, sep=',', year=None):
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
            extract_archive(f, outdir='./')
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

    yold = -1
    for file in flist:
        module_logger.info('Processing file: %s' % file)
        if 'csv' in file.lower():
            for enc in ['utf-8', 'utf-16', 'latin-1']:
                try:
                    df = pd.read_csv(file, sep=sep, dtype=dict_dtypes, header=0, encoding=enc, low_memory=False)
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

        df.columns = df.columns.str.upper()
        tgtcols = list(df.columns)
        regexp = re.compile('^DT|.*DOSE')
        dt_cols = list(filter(regexp.match, tgtcols))
        df = date_cleanup(df, dt_cols)

        write_to_folder(df[df.DT_SIN_PRI < '2021-01-03'], 2020, cwd)
        write_to_folder(df[(df.DT_SIN_PRI >= '2021-01-03') & (df.DT_SIN_PRI <= '2022-01-01')], 2021, cwd)
        write_to_folder(df[(df.DT_SIN_PRI >= '2022-01-02')], 2022, cwd)
        run(['rm', '-f', dir+file], check=True)

    os.chdir(cwd)
    return df


def main(dir, sep=',', year=None):

    df = extract_csv(dir, sep=sep, year=year)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Extract csv from zipped file sent by email.\n" +
                                                 "python3 email_extract.py --dir [folder] --year [YYYY]\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--dir', help='Base folder')
    parser.add_argument('--year', help='Base year')
    parser.add_argument('--sep', help='Column delimiter')
    args = parser.parse_args()
    print(args)
    main(args.dir, args.sep, args.year)
