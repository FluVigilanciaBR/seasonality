# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

# Apply filters of interest in a dataframe containing SINAN-SRAG data
import pandas as pd
import numpy as np
import argparse
import logging
import re
from argparse import RawDescriptionHelpFormatter
from .insert_epiweek import insert_epiweek
from .delay_table import extract_quantile, delayimputation, createtable

module_logger = logging.getLogger('update_system.sinan_filter_of_interest')
tabela_siglauf = {'RO': 11,
                 'AC': 12,
                 'AM': 13,
                 'RR': 14,
                 'PA': 15,
                 'AP': 16,
                 'TO': 17,
                 'MA': 21,
                 'PI': 22,
                 'CE': 23,
                 'RN': 24,
                 'PB': 25,
                 'PE': 26,
                 'AL': 27,
                 'SE': 28,
                 'BA': 29,
                 'MG': 31,
                 'ES': 32,
                 'RJ': 33,
                 'SP': 35,
                 'PR': 41,
                 'SC': 42,
                 'RS': 43,
                 'MS': 50,
                 'MT': 51,
                 'GO': 52,
                 'DF': 53,
                 'NI': 99}

filtro_dict = {1: '_hospdeath',
               2: '_sragnofever',
               3: ''}

dict_dtypes = {
    'NU_NOTIFIC': 'str',
    'DT_NOTIFIC': 'str',
    'SEM_NOT': 'float64',
    'DT_SIN_PRI': 'str',
    'SEM_PRI': 'float64',
    'SG_UF_NOT': 'str',
    'ID_REGIONA': 'str',
    'CO_REGIONA': 'float64',
    'ID_MUNICIP': 'str',
    'CO_MUN_NOT': 'float64',
    'ID_UNIDADE': 'str',
    'CO_UNI_NOT': 'float64',
    'NM_UN_INTE': 'str',
    'CO_UN_INTE': 'float64',
    'LAB_AN': 'str',
    'CO_LAB_AN': 'float64',
    'RES_CAPITAL': 'str',
    'NOT_CAPITAL': 'str',
    'NU_CPF': 'str',
    'NM_PACIENT': 'str',
    'CS_SEXO': 'str',
    'DT_NASC': 'str',
    'NU_IDADE_N': 'str',
    'TP_IDADE': 'float64',
    'COD_IDADE': 'str',
    'CS_GESTANT': 'float64',
    'CS_RACA': 'float64',
    'CS_ETINIA': 'str',
    'CS_ESCOL_N': 'float64',
    'NM_MAE_PAC': 'str',
    'NU_CEP': 'str',
    'ID_PAIS': 'str',
    'CO_PAIS': 'float64',
    'SG_UF': 'str',
    'ID_RG_RESI': 'str',
    'CO_RG_RESI': 'float64',
    'ID_MN_RESI': 'str',
    'CO_MUN_RES': 'float64',
    'NM_BAIRRO': 'str',
    'NM_LOGRADO': 'str',
    'NU_NUMERO': 'str',
    'NM_COMPLEM': 'str',
    'NU_DDD_TEL': 'float64',
    'NU_TELEFON': 'str',
    'CS_ZONA': 'float64',
    'SURTO_SG': 'float64',
    'NOSOCOMIAL': 'float64',
    'AVE_SUINO': 'float64',
    'FEBRE': 'float64',
    'TOSSE': 'float64',
    'GARGANTA': 'float64',
    'DISPNEIA': 'float64',
    'DESC_RESP': 'float64',
    'SATURACAO': 'float64',
    'DIARREIA': 'float64',
    'VOMITO': 'float64',
    'OUTRO_SIN': 'float64',
    'OUTRO_DES': 'str',
    'PUERPERA': 'float64',
    'FATOR_RISC': 'str',
    'CARDIOPATI': 'float64',
    'HEMATOLOGI': 'float64',
    'SIND_DOWN': 'float64',
    'HEPATICA': 'float64',
    'ASMA': 'float64',
    'DIABETES': 'float64',
    'NEUROLOGIC': 'float64',
    'PNEUMOPATI': 'float64',
    'IMUNODEPRE': 'float64',
    'RENAL': 'float64',
    'OBESIDADE': 'float64',
    'OBES_IMC': 'str',
    'OUT_MORBI': 'float64',
    'MORB_DESC': 'str',
    'VACINA': 'float64',
    'DT_UT_DOSE': 'str',
    'MAE_VAC': 'float64',
    'DT_VAC_MAE': 'str',
    'M_AMAMENTA': 'float64',
    'DT_DOSEUNI': 'str',
    'DT_1_DOSE': 'str',
    'DT_2_DOSE': 'str',
    'ANTIVIRAL': 'float64',
    'TP_ANTIVIR': 'float64',
    'OUT_ANTIV': 'str',
    'DT_ANTIVIR': 'str',
    'HOSPITAL': 'float64',
    'DT_INTERNA': 'str',
    'SG_UF_INTE': 'str',
    'ID_RG_INTE': 'str',
    'CO_RG_INTE': 'float64',
    'ID_MN_INTE': 'str',
    'CO_MU_INTE': 'float64',
    'UTI': 'float64',
    'DT_ENTUTI': 'str',
    'DT_SAIDUTI': 'str',
    'SUPORT_VEN': 'float64',
    'RAIOX_RES': 'float64',
    'RAIOX_OUT': 'str',
    'DT_RAIOX': 'str',
    'AMOSTRA': 'float64',
    'DT_COLETA': 'str',
    'TP_AMOSTRA': 'float64',
    'OUT_AMOST': 'str',
    'REQUI_GAL': 'float64',
    'IF_RESUL': 'float64',
    'DT_IF': 'str',
    'POS_IF_FLU': 'float64',
    'TP_FLU_IF': 'float64',
    'POS_IF_OUT': 'float64',
    'IF_VSR': 'float64',
    'IF_PARA1': 'float64',
    'IF_PARA2': 'float64',
    'IF_PARA3': 'float64',
    'IF_ADENO': 'float64',
    'IF_OUTRO': 'float64',
    'DS_IF_OUT': 'object',
    'LAB_IF': 'str',
    'CO_LAB_IF': 'float64',
    'PCR_RESUL': 'float64',
    'DT_PCR': 'str',
    'POS_PCRFLU': 'float64',
    'TP_FLU_PCR': 'float64',
    'PCR_FLUASU': 'float64',
    'FLUASU_OUT': 'str',
    'PCR_FLUBLI': 'float64',
    'FLUBLI_OUT': 'str',
    'POS_PCROUT': 'float64',
    'PCR_VSR': 'float64',
    'PCR_PARA1': 'float64',
    'PCR_PARA2': 'float64',
    'PCR_PARA3': 'float64',
    'PCR_PARA4': 'float64',
    'PCR_ADENO': 'float64',
    'PCR_METAP': 'float64',
    'PCR_BOCA': 'float64',
    'PCR_RINO': 'float64',
    'PCR_OUTRO': 'float64',
    'DS_PCR_OUT': 'str',
    'LAB_PCR': 'str',
    'CO_LAB_PCR': 'float64',
    'CLASSI_FIN': 'float64',
    'CLASSI_OUT': 'str',
    'CRITERIO': 'float64',
    'EVOLUCAO': 'float64',
    'DT_EVOLUCA': 'str',
    'NU_DO': 'float64',
    'DT_ENCERRA': 'str',
    'OBSERVA': 'str',
    'NOME_PROF': 'str',
    'REG_PROF': 'str',
    'DT_DIGITA': 'str',
    'HISTO_VGM': 'float64',
    'PAIS_VGM': 'str',
    'CO_PS_VGM': 'float64',
    'LO_PS_VGM': 'str',
    'DT_VGM': 'str',
    'DT_RT_VGM': 'str',
    'PCR_SARS2': 'float64',
    'PAC_COCBO': 'str',
    'PAC_DSCBO': 'str',
    'OUT_ANIM': 'str',
    'DOR_ABD': 'float64',
    'FADIGA': 'float64',
    'PERD_OLFT': 'float64',
    'PERD_PALA': 'float64',
    'TOMO_RES': 'float64',
    'TOMO_OUT': 'str',
    'DT_TOMO': 'str',
    'TP_TES_AN': 'float64',
    'DT_RES_AN': 'str',
    'RES_AN': 'float64',
    'POS_AN_FLU': 'float64',
    'TP_FLU_AN': 'float64',
    'POS_AN_OUT': 'float64',
    'AN_SARS2': 'float64',
    'AN_VSR': 'float64',
    'AN_PARA1': 'float64',
    'AN_PARA2': 'float64',
    'AN_PARA3': 'float64',
    'AN_ADENO': 'float64',
    'AN_OUTRO': 'float64',
    'DS_AN_OUT': 'str',
    'TP_AM_SOR': 'float64',
    'SOR_OUT': 'str',
    'DT_CO_SOR': 'str',
    'TP_SOR': 'float64',
    'OUT_SOR': 'str',
    'DT_RES': 'str',
    'RES_IGG': 'float64',
    'RES_IGM': 'float64',
    'RES_IGA': 'float64',
    'DT_ATUALIZACAO': 'str',
    '@VERSION': 'int64',
    '@TIMESTAMP': 'str'
}


def clean_open_field(s: pd.Series):

    specialchar = '\@|\$|\%|\&|\*'
    s = s.where(~s.str.contains(specialchar, regex=True, na=False), 'CENSURADO')

    return s


def check_covid19_regex(df: pd.DataFrame, column: str):

    if sum(~pd.isnull(df[column])) > 0:
        sars_cov_2_regex = 'novo corona|covid|coovid|cov-2|covd-19|' \
                           'cov2|cov 2|cov- 2|cov -2|cov - 2|c0v-2|cav-2|cav 2|arvs-cov|ars- cov|ars cov|ars - ' \
                           'cov|ars-cov|ars2cov|covi-19|civid 19|cobid-19|covis|cov-2|civid-19'
        sars_cov_2_regex_discard = 'nao detectado para covid19'
        row_filter = (df[column].str.contains(sars_cov_2_regex.upper(), regex=True, na=False)) & \
                     (~df[column].str.contains(sars_cov_2_regex_discard.upper(), regex=True, na=False))
    else:
        row_filter = ~pd.isnull(df[column])

    return row_filter


def readtable(fname, sep=','):
    try:
        df = pd.read_csv(fname, sep=sep, dtype=dict_dtypes, low_memory=False, encoding='utf-8')
    except:
        df = pd.read_csv(fname, sep=sep, dtype=dict_dtypes, low_memory=False, encoding='utf-16')

    return df


def date_cleanup(df, dt_cols):
    '''
    Standardize column data and discard those without notification and/or first symptoms date.
    :param df: notifications data frame
    :param dt_cols: list of data columns
    :return: df: standardized data frame
    '''

    # Filter by notification date
    df = df.where(df != -1, None)
    df[dt_cols] = df[dt_cols].where(df[dt_cols] != 10101, None)
    if df.CRITERIO.dtype == 'O':
        df.CRITERIO = df.CRITERIO.where(df.CRITERIO != 'NÃ', None)
    df.dropna(subset=["DT_SIN_PRI", "DT_NOTIFIC"], inplace=True)

    # Convert all date related columns to datetime format
    for col in dt_cols:
        # Convert all date columns to datetime format. Output will have the format YYYY-MM-DD
        dtformat = '%Y%m%d'
        if sum(~pd.isnull(df[col])) > 0:
            sample = df.loc[~pd.isnull(df[col]), col].values[0]
            if isinstance(sample, str):
                if 'T' in sample:
                    df[col] = pd.to_datetime(df[col].str[:10], errors='coerce', format='%Y-%m-%d')
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
    # Discard those neither hospitalized nor deceased. For cases from 2009, keep all:
    df = df[(df.DT_SIN_PRI.apply(lambda x: x.year) == 2009) | (df.HOSPITAL == 1) | (df.EVOLUCAO == 2)]

    return df


def table_compatibility(df):
    '''
    Create column conversion to make old and new data compatible
    :param df: input data frame
    :return:
    '''

    # Create equivalence between 2020-07-27 new variables and old ones:
    df[['DT_IF', 'IF_RESUL']] = df[['DT_RES_AN', 'RES_AN']].copy()

    l_new = ['CO_LAB_AN',
             'POS_AN_FLU',
             'TP_FLU_AN',
             'POS_AN_OUT',
             'AN_VSR',
             'AN_PARA1',
             'AN_PARA2',
             'AN_PARA3',
             'AN_ADENO',
             'AN_OUTRO',
             'DS_AN_OUT']
    cols = df.columns
    col_new = []
    col_old = []
    l_intersec = set(l_new).intersection(cols)
    for li in l_intersec:
        col_new.append(li.replace('AN', 'IF'))
        col_old.append(li)

    if len(l_intersec) > 0:
        df[col_new] = df[col_old].copy()

    return


def symptoms_filter(df, filtertype='srag'):

    # Filter by symptoms:
    # All cases, regardless of year, must either attend symptoms definition or have evolved to
    # death. Besides that, cases from 2009 do not need hospitalization to be considered as true case, per Ministry
    # of Health request. For all other years, they must have been hospitalized or have evolved to death.
    # The filter regarding hospitalization and case evolution is done after date columns consolidation to attend
    # 2009's particularity.

    # all
    df['filtro'] = 0
    # hospdeath:
    df.loc[(df.EVOLUCAO == 2) | (df.HOSPITAL == 1), 'filtro'] = 1
    # sragnofever:
    df.loc[(df.filtro == 1) & (
            ((df.TOSSE == 1) | (df.GARGANTA == 1)) &
            ((df.DISPNEIA == 1) | (df.SATURACAO == 1) | (df.DESC_RESP == 1))
    ), 'filtro'] = 2
    # srag:
    df.loc[(df.filtro == 2) & (df.FEBRE == 1), 'filtro'] = 3

    # if filtertype == 'srag':
    #     df = df[(
    #                 (df.FEBRE == 1) &
    #                 ((df.TOSSE == 1) | (df.GARGANTA == 1)) &
    #                 ((df.DISPNEIA == 1) | (df.SATURACAO == 1) | (df.DESC_RESP == 1))
    #             )].copy()
    # elif filtertype == 'sragnofever':
    #     df = df[(
    #                 ((df.TOSSE == 1) | (df.GARGANTA == 1)) &
    #                 ((df.DISPNEIA == 1) | (df.SATURACAO == 1) | (df.DESC_RESP == 1))
    #             )].copy()

    return df


def filter_db2019(df, tag=None, filtertype='srag'):
    tgtcols = ['AMOSTRA', 'ANTIVIRAL', 'AVE_SUINO', 'CLASSI_FIN', 'CLASSI_OUT', 'CO_LAB_IF', 'CO_LAB_PCR', 'CO_MU_INTE',
               'CO_MUN_NOT', 'CO_MUN_RES', 'CO_PAIS', 'CO_REGIONA', 'CO_RG_RESI', 'CO_UNI_NOT', 'CRITERIO', 'CS_ETINIA',
               'CO_UN_INTE',
               'CS_RACA', 'CS_SEXO', 'DS_IF_OUT', 'DS_PCR_OUT', 'DT_ANTIVIR',
               'DT_COLETA', 'DT_DIGITA', 'DT_ENCERRA', 'DT_EVOLUCA', 'DT_IF', 'DT_INTERNA', 'DT_NOTIFIC', 'DT_PCR',
               'DT_IFI',
               'DT_SIN_PRI', 'DT_UT_DOSE', 'EVOLUCAO', 'FLUASU_OUT', 'FLUBLI_OUT', 'HOSPITAL',
               'ID_MN_RESI', 'ID_MUNICIP', 'ID_REGIONA', 'ID_RG_RESI', 'IF_ADENO', 'IF_OUTRO', 'IF_PARA1',
               'IF_PARA2', 'IF_PARA3', 'IF_RESUL', 'IF_VSR', 'NU_CEP', 'NU_IDADE_N', 'NU_NOTIFIC', 'NM_BAIRRO',
               'NM_LOGRADO', 'OUT_ANTIVIR', 'PCR_ADENO', 'PCR_BOCA', 'PCR_FLUASU', 'PCR_FLUBLI', 'PCR_METAP',
               'PCR_OUTRO', 'PCR_PARA1',
               'PCR_PARA2', 'PCR_PARA3', 'PCR_PARA4', 'PCR_RESUL', 'PCR_RINO', 'PCR_SARS2', 'PCR_VSR', 'POS_IF_FLU',
               'POS_IF_OUT', 'POS_PCRFLU', 'POS_PCROUT', 'SEM_NOT', 'SEM_PRI', 'SG_UF', 'SG_UF_NOT',
               'TP_ANTIVIR', 'TP_FLU_IF', 'TP_FLU_PCR', 'TP_IDADE', 'VACINA']

    tgt_cols_sintomas = ['DESC_RESP',
                         'DISPNEIA',
                         'DIARREIA',
                         'FEBRE',
                         'GARGANTA',
                         'TOSSE',
                         'SATURACAO',
                         'DIARREIA',
                         'VOMITO',
                         'DOR_ABD',
                         'FADIGA',
                         'PERD_OLFT',
                         'PERD_PALA',
                         'OUTRO_SIN',
                         'OUTRO_DES']

    tgtcols_uti = ['UTI',
                   'DT_ENTUTI',
                   'DT_SAIDUTI',
                   'SUPORT_VEN']

    tgtcols_comorb = ['CS_GESTANT',
                      'FATOR_RISC',
                      'PUERPERA',
                      'CARDIOPATI',
                      'HEMATOLOGI',
                      'HEPATICA',
                      'SIND_DOWN',
                      'HEPATICA',
                      'ASMA',
                      'DIABETES',
                      'NEUROLOGIC',
                      'PNEUMOPATI',
                      'IMUNODEPRE',
                      'RENAL',
                      'OBESIDADE',
                      'OBES_IMC',
                      'OUT_MORBI',
                      'MORB_DESC']

    tgtcols_2020_07_27 = ['TP_TES_AN',
                          'DT_RES_AN',
                          'RES_IGA',
                          'RES_IGG',
                          'RES_IGM',
                          'LAB_AN',
                          'CO_LAB_AN',
                          'POS_AN_FLU',
                          'TP_FLU_AN',
                          'POS_AN_OUT',
                          'AN_SARS2',
                          'AN_VSR',
                          'AN_PARA1',
                          'AN_PARA2',
                          'AN_PARA3',
                          'AN_ADENO',
                          'AN_OUTRO',
                          'DS_AN_OUT',
                          'SUPORT_VEN',
                          'RAIOX_RES',
                          'RAIOX_OUT',
                          'DT_RAIOX',
                          'TOMO_RES',
                          'TOMO_OUT',
                          'DT_TOMO']

    if 'DT_RES_AN' in df.columns:
        table_compatibility(df)

    tgtcols = list(set(tgtcols).union(tgt_cols_sintomas).union(tgtcols_uti).union(tgtcols_comorb).union(tgtcols_2020_07_27))
    if 'DT_IF' in df.columns:
        df['DT_IFI'] = df.DT_IF

    cols = df.columns
    for col in set(tgtcols).difference(cols):
        df[col] = None

    df = df[tgtcols].copy()
    df = symptoms_filter(df, filtertype=filtertype)

    regexp = re.compile('^DT')
    dt_cols = list(filter(regexp.match, tgtcols))
    df = date_cleanup(df, dt_cols)

    # Registro da idade, em anos.
    # Campo TP_IDADE indica a escala da variável NU_IDADE_N, sendo:
    # 1-Dias, 2-Meses, 3-Anos
    df['idade_em_anos'] = df.NU_IDADE_N.where(df.TP_IDADE == 3, 0)

    # If sample collection field is empty, convert to unknown:
    df.AMOSTRA = df.AMOSTRA.where(pd.notnull(df.AMOSTRA), 9).astype(int)

    # Convert UF abbreviation to numeric code:
    df.loc[pd.notnull(df.SG_UF_NOT), 'SG_UF_NOT'] = df.loc[pd.notnull(df.SG_UF_NOT), 'SG_UF_NOT'].map(tabela_siglauf)
    df.loc[pd.isnull(df.SG_UF_NOT), 'SG_UF_NOT'] = 99
    df.loc[pd.notnull(df.SG_UF), 'SG_UF'] = df.loc[pd.notnull(df.SG_UF), 'SG_UF'].map(tabela_siglauf)
    df.loc[pd.isnull(df.SG_UF), 'SG_UF'] = 99

    # Clean up PCR_RESUL and IF_RESUL fields:
    def labresultcleanup(dfin, x):
        # If sample was collected, IF/PCR result cannot be unknown or empty:
        dfin.loc[(dfin.AMOSTRA == 1) & (~dfin[x].isin([1, 2, 3, 4])), x] = 5
        # IF PCR/IF result field is marked unknown but sample was not collected, convert to not tested:
        dfin.loc[(dfin[x] == 9) & (dfin.AMOSTRA == 2), x] = 4
        # If PCR/IF result field is empty and sample was NOT collected, convert to not tested
        dfin[x] = dfin[x].where(pd.notnull(dfin[x]) | (dfin.AMOSTRA != 2), 4)
        # If PCR/IF result field is empty and sample field is empty or unknown, convert to unknown
        dfin[x] = dfin[x].where(pd.notnull(dfin[x]) | (dfin.AMOSTRA.isin([1, 2])), 9)

        return

    labresultcleanup(df, 'PCR_RESUL')
    labresultcleanup(df, 'IF_RESUL')

    df['FLU_A'] = None
    df['FLU_B'] = None
    df['FLU_LAB'] = None
    df['VSR'] = None
    df['PARA1'] = None
    df['PARA2'] = None
    df['PARA3'] = None
    df['PARA4'] = None
    df['ADNO'] = None
    df['METAP'] = None
    df['BOCA'] = None
    df['RINO'] = None
    df['SARS2'] = None
    df['OTHERS'] = None

    df['NEGATIVE'] = None
    df['POSITIVE'] = None
    df['INCONCLUSIVE'] = None
    df['DELAYED'] = None
    df['TESTED'] = None
    df['NOTTESTED'] = None
    df['TESTING_IGNORED'] = None

    def labresult(x, y=None, pos=1):
        if all(pd.isnull([x, y])):
            return None
        elif pos in [x, y]:
            return 1
        else:
            return 0

    df.FLU_A = df[['TP_FLU_PCR', 'TP_FLU_IF']].apply(lambda s: labresult(s[0], s[1]), axis=1)
    df.FLU_B = df[['TP_FLU_PCR', 'TP_FLU_IF']].apply(lambda s: labresult(s[0], s[1], pos=2), axis=1)
    df.loc[(df.POS_IF_FLU == 1) | (df.POS_PCRFLU == 1) | (df.FLU_A == 1) | (df.FLU_B == 1) |
           ((df.CLASSI_FIN == 1) & (df.CRITERIO == 1)), 'FLU_LAB'] = 1
    df.loc[(df.FLU_LAB != 1) & ((df.POS_IF_FLU == 2) | (df.POS_PCRFLU == 2)), 'FLU_LAB'] = 0
    df.loc[(df.IF_VSR == 1) | (df.PCR_VSR == 1), 'VSR'] = 1
    df.loc[(df.IF_PARA1 == 1) | (df.PCR_PARA1 == 1), 'PARA1'] = 1
    df.loc[(df.IF_PARA2 == 1) | (df.PCR_PARA2 == 1), 'PARA2'] = 1
    df.loc[(df.IF_PARA3 == 1) | (df.PCR_PARA3 == 1), 'PARA3'] = 1
    df.loc[(df.PCR_PARA4 == 1), 'PARA4'] = 1
    df.loc[(df.IF_ADENO == 1) | (df.PCR_ADENO == 1), 'ADNO'] = 1
    df.loc[(df.PCR_BOCA == 1), 'BOCA'] = 1
    df.loc[(df.PCR_RINO == 1), 'RINO'] = 1
    df.loc[(df.PCR_METAP == 1), 'METAP'] = 1
    df.loc[(df.IF_OUTRO == 1) | (df.PCR_OUTRO == 1), 'OTHERS'] = 1

    mask_covid19_ds_pcr = check_covid19_regex(df, 'DS_PCR_OUT')
    mask_covid19_ds_if = check_covid19_regex(df, 'DS_IF_OUT')

    df.loc[(df.PCR_SARS2 == 1) |
           (df.AN_SARS2 == 1) |
           mask_covid19_ds_pcr |
           mask_covid19_ds_if |
           ((df.CLASSI_FIN == 5) & (df.CRITERIO == 1)),
           'SARS2'] = 1
    df.loc[(df.PCR_SARS2 == 1) |
           mask_covid19_ds_pcr |
           mask_covid19_ds_if,
           'OTHERS'] = None

    # Positive cases:
    df.loc[(df.POS_PCRFLU == 1) | (df.POS_PCROUT == 1), 'PCR_RESUL'] = 1
    df.loc[(df.POS_IF_FLU == 1) | (df.POS_IF_OUT == 1), 'IF_RESUL'] = 1
    df.loc[(df.PCR_RESUL == 1) |
           (df.IF_RESUL == 1) |
           (df.FLU_LAB == 1) |
           (df.SARS2 == 1) |
           (df.VSR == 1) |
           (df.PARA1 == 1) |
           (df.PARA2 == 1) |
           (df.PARA3 == 1) |
           (df.PARA4 == 1) |
           (df.ADNO == 1) |
           (df.BOCA == 1) |
           (df.RINO == 1) |
           (df.METAP == 1) |
           (df.OTHERS == 1),
           'POSITIVE'] = 1

    # Negative cases:
    df.loc[(df.POS_PCRFLU == 2) & (df.POS_PCROUT == 2), 'PCR_RESUL'] = 2
    df.loc[(df.POS_IF_FLU == 2) & (df.POS_IF_OUT == 2), 'IF_RESUL'] = 2
    mask = (
            (df.POSITIVE != 1) &
            ((df.FLU_LAB == 0) | (df.POS_PCROUT == 2) | (df.POS_IF_OUT == 2))
    )
    df.loc[mask, 'NEGATIVE'] = 1

    df.loc[(((df.IF_RESUL == 2) & (df.PCR_RESUL.isin([2, 3, 4, 5, 9]))) |
           ((df.PCR_RESUL == 2) & (df.IF_RESUL.isin([2, 3, 4, 5, 9])))) &
           (df.POSITIVE != 1), 'NEGATIVE'] = 1
    df.loc[:, 'INCONCLUSIVE'] = (
                                    (
                                            ((df.IF_RESUL == 3) & (~df.PCR_RESUL.isin([1, 2, 5]))) |
                                            ((df.PCR_RESUL == 3) & (~df.IF_RESUL.isin([1, 2, 5])))
                                    ) &
                                    (df.POSITIVE != 1) &
                                    (df.NEGATIVE != 1)
    ).astype(int)
    df.loc[(df.POSITIVE == 1) | (df.NEGATIVE == 1) | (df.INCONCLUSIVE == 1), 'TESTED'] = 1

    df.loc[:, 'DELAYED'] = (
                               (
                                       ((df.IF_RESUL == 5) & (pd.isnull(df.TESTED))) |
                                       ((df.PCR_RESUL == 5) & (pd.isnull(df.TESTED)))
                               ) &
                               (df.POSITIVE != 1) &
                               (df.NEGATIVE != 1) &
                               (df.INCONCLUSIVE != 1)
    ).astype(int)

    # Clinical and clinical-epidemiological diagnose:
    df['FLU_CLINIC'] = ((df.POS_IF_FLU != 1) & (df.POS_PCRFLU != 1) & (df.CLASSI_FIN == 1) &
                        (df.CRITERIO.isin([2, 3]))).astype(int)

    notknownrows = (
                           ((df.PCR_RESUL == 9) | pd.isnull(df.PCR_RESUL)) &
                           ((df.IF_RESUL == 9) | pd.isnull(df.IF_RESUL)) &
                           (df.POSITIVE != 1) &
                           (df.NEGATIVE != 1) &
                           (df.DELAYED != 1) &
                           (df.INCONCLUSIVE != 1)
    )
    nottestedrows = (
            ~(notknownrows) &
            (df.PCR_RESUL.isin([4, 9])) &
            (df.IF_RESUL.isin([4, 9])) &
            (df.TESTED != 1)
    )
    df['NOTTESTED'] = nottestedrows.astype(int)
    df['TESTING_IGNORED'] = notknownrows.astype(int)

    if tag:
        df['tag'] = tag

    return df


def applysinanfilter(df, tag=None, filtertype='srag'):
    # Filter columns of interest
    # Na solicitação, além das variáveis abaixo, necessitamos também do ID do caso
    tgtcols = ['SEM_NOT', 'DT_NOTIFIC', 'SG_UF_NOT', 'DT_INTERNA', 'DT_SIN_PRI', 'DT_DIGITA', 'HOSPITAL',
               'FEBRE', 'CLASSI_FIN', 'CRITERIO', 'SG_UF', 'ID_MN_RESI', 'ID_RG_RESI', 'SEM_PRI',
               'TOSSE', 'GARGANTA', 'DISPNEIA', 'SATURACAO', 'DESC_RESP', 'EVOLUCAO', 'DT_COLETA', 'IFI', 'DT_IFI',
               'PCR', 'OUT_METODO', 'DS_OUTMET', 'DT_OUTMET', 'RES_FLUA', 'RES_FLUASU', 'RES_FLUB',
               'RES_VSR', 'RES_PARA1', 'RES_PARA2', 'RES_PARA3', 'RES_ADNO', 'RES_OUTRO', 'DT_PCR', 'PCR_RES',
               'PCR_ETIOL', 'PCR_TIPO_H', 'PCR_TIPO_N', 'DT_CULTURA', 'CULT_RES', 'DT_HEMAGLU', 'HEMA_RES',
               'HEMA_ETIOL', 'HEM_TIPO_H', 'HEM_TIPO_N', 'VACINA', 'DT_UT_DOSE', 'ANT_PNEUMO', 'DT_PNEUM',
               'CO_UF_INTE', 'CO_MU_INTE', 'CO_UN_INTE', 'DT_ENCERRA', 'NU_NOTIFIC', 'ID_AGRAVO', 'ID_MUNICIP',
               'ID_REGIONA', 'ID_UNIDADE', 'NU_IDADE_N', 'NM_BAIRRO', 'NM_LOGRADO', 'CS_SEXO', 'CS_RACA',
               'DT_ANTIVIR', 'DT_EVOLUCA']

    tgtcols_uti = ['UTI',
                   'DT_ENTUTI',
                   'DT_SAIDUTI']

    tgtcols_comorb = ['CS_GESTANT',
                      'PUERPERA',
                      'CARDIOPATI',
                      'HEMATOLOGI',
                      'SIND_DOWN',
                      'HEPATICA',
                      'ASMA',
                      'DIABETES',
                      'NEUROLOGIC',
                      'PNEUMOPATI',
                      'IMUNODEPRE',
                      'RENAL',
                      'OBESIDADE',
                      'OBES_IMC',
                      'OUT_MORBI',
                      'MORB_DESC']

    tgtcols = list(set(tgtcols).union(tgtcols_uti).union(tgtcols_comorb))

    cols = df.columns
    if 'RES_VRS' in cols:
        df.rename(columns={'RES_VRS': 'RES_VSR'}, inplace=True)
    if 'DT_PCR_1' in cols:
        df.DT_PCR.update(df.DT_PCR_1)
    if 'DT_OBITO' in cols:
        df.rename(columns={'DT_OBITO': 'DT_EVOLUCA'}, inplace=True)
        if 'DT_EVOL' in cols:
            df.DT_EVOLUCA.update(df.DT_EVOL)
    elif 'DT_EVOL' in cols:
        df.rename(columns={'DT_EVOL': 'DT_EVOLUCA'}, inplace=True)
    if 'METABOLICA' in cols:
        df.rename(columns={'METABOLICA': 'DIABETES'}, inplace=True)

    cols = df.columns
    for col in set(tgtcols).difference(cols):
        df[col] = None

    df = df[tgtcols].copy()

    df = symptoms_filter(df, filtertype=filtertype)

    regexp = re.compile('^DT')
    dt_cols = list(filter(regexp.match, tgtcols))
    df = date_cleanup(df, dt_cols)

    # Create columns related to lab result
    # Rows with lab test:
    labrows = ((df.PCR_RES.isin([1, 2, 3])) |
               (df.CULT_RES.isin([1, 2])) |
               (df.HEMA_RES.isin([1, 2, 3])) |
               (df.IFI == 1) |
               (df.PCR == 1) |
               (df.OUT_METODO == 1))

    notknownrows = (
        pd.isnull(df.PCR_RES) &
        pd.isnull(df.CULT_RES) &
        pd.isnull(df.HEMA_RES) &
        pd.isnull(df.IFI) &
        pd.isnull(df.PCR) &
        pd.isnull(df.OUT_METODO)
    )

    nottestedrows = (
        ~(notknownrows) &
        (pd.isnull(df.PCR_RES) | (df.PCR_RES.isin([4]))) &
        (pd.isnull(df.CULT_RES) | (df.CULT_RES.isin([3]))) &
        (pd.isnull(df.HEMA_RES) | (df.HEMA_RES.isin([4]))) &
        (pd.isnull(df.IFI) | (df.IFI == 2)) &
        (pd.isnull(df.PCR) | (df.PCR == 2)) &
        (pd.isnull(df.OUT_METODO) | (df.OUT_METODO == 2))
    )

    df['FLU_A'] = None
    df['FLU_B'] = None
    df['VSR'] = None
    df['PARA1'] = None
    df['PARA2'] = None
    df['PARA3'] = None
    df['PARA4'] = None
    df['ADNO'] = None
    df['METAP'] = None
    df['BOCA'] = None
    df['RINO'] = None
    df['SARS2'] = None

    df['OTHERS'] = None
    df['NEGATIVE'] = None
    df['INCONCLUSIVE'] = None
    df['DELAYED'] = None

    df['TESTED'] = labrows.astype(int)
    df['NOTTESTED'] = nottestedrows.astype(int)
    df['TESTING_IGNORED'] = notknownrows.astype(int)

    df.loc[labrows, 'FLU_A'] = ((df.PCR_ETIOL[labrows].isin([1, 2, 4])) | (df.HEMA_ETIOL[labrows].isin([1, 2, 4])) |
                                (df.RES_FLUA[labrows] == 1)).astype(int)
    df.loc[labrows, 'FLU_B'] = ((df.PCR_ETIOL[labrows] == 3) | (df.HEMA_ETIOL[labrows] == 3) |
                                (df.RES_FLUB[labrows] == 1)).astype(int)
    df.loc[labrows, 'VSR'] = (df.RES_VSR[labrows] == 1).astype(int)
    df.loc[labrows, 'PARA1'] = (df.RES_PARA1[labrows] == 1).astype(int)
    df.loc[labrows, 'PARA2'] = (df.RES_PARA2[labrows] == 1).astype(int)
    df.loc[labrows, 'PARA3'] = (df.RES_PARA3[labrows] == 1).astype(int)
    df.loc[labrows, 'ADNO'] = (df.RES_ADNO[labrows] == 1).astype(int)
    df.loc[labrows, 'SARS2'] = 0

    df.loc[labrows, 'OTHERS'] = (
        (df.PCR_ETIOL[labrows] == 5) |
        (df.HEMA_ETIOL[labrows] == 5) |
        (df.RES_OUTRO[labrows] == 1)).astype(int)
    df.loc[labrows, 'DELAYED'] = ((pd.isnull(df.PCR_RES[labrows]) | df.PCR_RES[labrows] == 4) &
                                  (pd.isnull(df.HEMA_RES[labrows]) | df.HEMA_RES[labrows] == 4) &
                                  (pd.isnull(df.RES_FLUA[labrows]) | df.RES_FLUA[labrows] == 4) &
                                  (pd.isnull(df.RES_FLUB[labrows]) | df.RES_FLUB[labrows] == 4) &
                                  (pd.isnull(df.RES_VSR[labrows]) | df.RES_VSR[labrows] == 4) &
                                  (pd.isnull(df.RES_PARA1[labrows]) | df.RES_PARA1[labrows] == 4) &
                                  (pd.isnull(df.RES_PARA2[labrows]) | df.RES_PARA2[labrows] == 4) &
                                  (pd.isnull(df.RES_PARA3[labrows]) | df.RES_PARA3[labrows] == 4) &
                                  (pd.isnull(df.RES_ADNO[labrows]) | df.RES_ADNO[labrows] == 4) &
                                  (pd.isnull(df.RES_OUTRO[labrows]) | df.RES_OUTRO[labrows] == 4)).astype(int)
    df.loc[labrows, 'INCONCLUSIVE'] = ((df.DELAYED[labrows] == 0) &
                                       (pd.isnull(df.PCR_RES[labrows]) | df.PCR_RES[labrows].isin([3, 4])) &
                                       (pd.isnull(df.HEMA_RES[labrows]) | df.HEMA_RES[labrows].isin([3, 4])) &
                                       (pd.isnull(df.RES_FLUA[labrows]) | df.RES_FLUA[labrows].isin([3, 4])) &
                                       (pd.isnull(df.RES_FLUB[labrows]) | df.RES_FLUB[labrows].isin([3, 4])) &
                                       (pd.isnull(df.RES_VSR[labrows]) | df.RES_VSR[labrows].isin([3, 4])) &
                                       (pd.isnull(df.RES_PARA1[labrows]) | df.RES_PARA1[labrows].isin([3, 4])) &
                                       (pd.isnull(df.RES_PARA2[labrows]) | df.RES_PARA2[labrows].isin([3, 4])) &
                                       (pd.isnull(df.RES_PARA3[labrows]) | df.RES_PARA3[labrows].isin([3, 4])) &
                                       (pd.isnull(df.RES_ADNO[labrows]) | df.RES_ADNO[labrows].isin([3, 4])) &
                                       (pd.isnull(df.RES_OUTRO[labrows]) | df.RES_OUTRO[labrows].isin([3, 4]))).astype(
        int)
    df.loc[labrows, 'NEGATIVE'] = ((df.FLU_A[labrows] == 0) & (df.FLU_B[labrows] == 0) & (df.VSR[labrows] == 0) &
                                   (df.PARA1[labrows] == 0) & (df.PARA2[labrows] == 0) & (df.PARA3[labrows] == 0) &
                                   (df.ADNO[labrows] == 0) & (df.OTHERS[labrows] == 0) & (df.DELAYED[labrows] == 0) &
                                   (df.INCONCLUSIVE[labrows] == 0)).astype(int)
    df.loc[labrows & (df.INCONCLUSIVE != 1) & (df.NEGATIVE != 1) & (df.DELAYED != 1), 'POSITIVE'] = 1

    # Clinical and clinical-epidemiological diagnose:
    df['FLU_CLINIC'] = ((df.FLU_A != 1) & (df.FLU_B != 1) & (df.CLASSI_FIN == 1) & (df.CRITERIO.isin([2, 3]))).astype(
        int)

    df.NU_IDADE_N = df.NU_IDADE_N.astype(np.float)

    def f_idade(x):

        # System registers age with the following format:
        # TAAA
        # T: type (1-hours, 2-days, 3-months, 4-years)
        #   Hours used only if age < 24h, days used only if 24h <= age < 30d, months only if 30d <= a < 12 months
        # AAA: count on the respective scale
        # Ex.:
        # 2010 : 10 days
        # 3010: 10 months
        # 4010: 10 years
        if np.isnan(x):
            return np.nan
        if x < 4000:
            return 0
        else:
            return x - 4000

    if (df.NU_IDADE_N > 4000).any(axis = 0):
        df['idade_em_anos'] = df['NU_IDADE_N'].apply(f_idade)
    else:
        df['idade_em_anos'] = df['NU_IDADE_N']
    if tag:
        df['tag'] = tag

    return df


def delays_dist(df_in=pd.DataFrame(), filtertype='srag', append_file=None, dfappend=None):
    module_logger.info('Entered function: delay_dist')
    
    if filtertype not in ['srag', 'sragnofever', 'hospdeath']:
        exit('Invalid filter type: %s' % filtertype)
        
    suff = ''
    if filtertype != 'srag':
        suff = '_%s' % filtertype
        
    df_reg = pd.read_csv('../data/regioesclimaticas.csv', low_memory=False)[['Código', 'Região', 'Região oficial']]
    df_reg.rename(columns={'Código': 'UF', 'Região': 'Regional', 'Região oficial': 'Regiao'}, inplace=True)

    def _cleanup(df_input):
        tgt_cols = ['SG_UF_NOT',
                    'DT_NOTIFIC_epiyearweek',
                    'DT_NOTIFIC_epiyear',
                    'DT_NOTIFIC_epiweek',
                    'DT_SIN_PRI_epiyearweek',
                    'DT_SIN_PRI_epiyear',
                    'DT_SIN_PRI_epiweek',
                    'DT_DIGITA_epiyearweek',
                    'DT_DIGITA_epiyear',
                    'DT_DIGITA_epiweek',
                    'Notific2Digita_DelayWeeks',
                    'SinPri2Digita_DelayWeeks',
                    'SinPri2Antivir_DelayWeeks',
                    'SinPri2Notific_DelayWeeks',
                    'SinPri2Coleta_DelayWeeks',
                    'SinPri2Interna_DelayWeeks',
                    'Notific2Encerra_DelayWeeks',
                    'Coleta2IFI_DelayWeeks',
                    'Coleta2PCR_DelayWeeks',
                    'Notific2Coleta_DelayWeeks',
                    'Notific2Antivir_DelayWeeks',
                    'Digita2Antivir_DelayWeeks',
                    'Interna2Evoluca_DelayWeeks',

                    'Notific2Digita_DelayDays',
                    'SinPri2Digita_DelayDays',
                    'SinPri2Antivir_DelayDays',
                    'SinPri2Notific_DelayDays',
                    'SinPri2Coleta_DelayDays',
                    'SinPri2Interna_DelayDays',
                    'Notific2Encerra_DelayDays',
                    'Coleta2IFI_DelayDays',
                    'Coleta2PCR_DelayDays',
                    'Notific2Coleta_DelayDays',
                    'Notific2Antivir_DelayDays',
                    'Digita2Antivir_DelayDays',
                    'Interna2Evoluca_DelayDays',

                    'sragflu',
                    'obitoflu',
                    'sragcovid',
                    'obitocovid',
                    'obito',
                    'filtro']
        df = df_input[tgt_cols].rename(columns={'DT_NOTIFIC_epiyear': 'epiyear',
                                                'DT_NOTIFIC_epiweek': 'epiweek',
                                                'SG_UF_NOT': 'UF'})

        df.loc[pd.isnull(df.UF), 'UF'] = 99
        df.UF = df.UF.astype(float).astype(int)
        # Insert region info:
        df = df.merge(df_reg, on='UF')
        df['Pais'] = 'BR'

        cols = ['DT_DIGITA_epiyear', 'DT_DIGITA_epiweek', 'SinPri2Digita_DelayWeeks']
        try:
            df[cols] = df[cols].astype(int)
        except:
            df[cols] = df[cols].astype(float)
        cols = ['epiyear', 'epiweek']
        try:
            df[cols] = df[cols].astype(int)
        except:
            df[cols] = df[cols].astype(float)
        cols = ['DT_SIN_PRI_epiyear', 'DT_SIN_PRI_epiweek']
        try:
            df[cols] = df[cols].astype(int)
        except:
            df[cols] = df[cols].astype(float)
        return df

    df = _cleanup(df_in)

    def wide2long(dfwide2long):

        dfwide2long['id'] = df.index
        dfwide2long['srag'] = 1
        varcols = ['srag',
                   'sragflu',
                   'obitoflu',
                   'sragcovid',
                   'obitocovid',
                   'obito']
        dfwide2long_out = dfwide2long[['id'] + varcols].melt(id_vars='id',
                                                             var_name='dado',
                                                             value_name='valor')[lambda x: (x.valor == 1)]
        dfwide2long_out = dfwide2long_out.merge(dfwide2long, how='left', on='id').drop(columns=['valor', 'id']+varcols)
        dfwide2long.drop(columns=['id', 'srag'], inplace=True)

        return dfwide2long_out

    df_out = wide2long(df)

    out_cols = ['Coleta2IFI_DelayDays', 'Coleta2IFI_DelayWeeks', 'Coleta2PCR_DelayDays',
                'Coleta2PCR_DelayWeeks', 'DT_DIGITA_epiweek', 'DT_DIGITA_epiyear',
                'DT_DIGITA_epiyearweek', 'DT_NOTIFIC_epiyearweek', 'DT_SIN_PRI_epiweek',
                'DT_SIN_PRI_epiyear', 'DT_SIN_PRI_epiyearweek',
                'Digita2Antivir_DelayDays', 'Digita2Antivir_DelayWeeks',
                'Notific2Antivir_DelayDays', 'Notific2Antivir_DelayWeeks',
                'Notific2Coleta_DelayDays', 'Notific2Coleta_DelayWeeks',
                'Notific2Digita_DelayDays', 'Notific2Digita_DelayWeeks',
                'Notific2Encerra_DelayDays', 'Notific2Encerra_DelayWeeks',
                'SinPri2Antivir_DelayDays', 'SinPri2Antivir_DelayWeeks',
                'SinPri2Coleta_DelayDays', 'SinPri2Coleta_DelayWeeks',
                'SinPri2Digita_DelayDays', 'SinPri2Digita_DelayWeeks',
                'SinPri2Notific_DelayDays', 'SinPri2Notific_DelayWeeks',
                'SinPri2Interna_DelayDays', 'SinPri2Interna_DelayWeeks',
                'Interna2Evoluca_DelayDays', 'Interna2Evoluca_DelayWeeks',
                'UF', 'dado', 'filtro',
                'epiweek', 'epiyear', 'Regional', 'Regiao', 'Pais']

    df_out = df_out.sort_values(by=['dado', 'UF', 'DT_SIN_PRI_epiyearweek', 'DT_NOTIFIC_epiyearweek',
                                    'DT_DIGITA_epiyearweek'])
    df_out = df_out.reset_index()[out_cols]
    if append_file:
        tmp = pd.read_csv(append_file)
        df_out = tmp.append(df_out[out_cols], ignore_index=True, sort=False)

    for k, suffix in filtro_dict.items():
        df_out.loc[(df_out.filtro >= k), out_cols].to_csv('../../data/data/delay_table%s.csv' % suffix, index=False)

    #### Opportunities estimation
    tgt_cols = ['UF', 'Regional', 'Regiao', 'Pais', 'dado', 'filtro', 'DT_SIN_PRI_epiyearweek', 'DT_SIN_PRI_epiyear',
                'DT_SIN_PRI_epiweek', 'SinPri2Digita_DelayWeeks', 'DT_DIGITA_epiyearweek', 'DT_DIGITA_epiyear',
                'DT_DIGITA_epiweek']
    # Generate quantiles' file:
    df_out = df_out[tgt_cols].rename(columns={'DT_SIN_PRI_epiyearweek': 'epiyearweek', 'DT_SIN_PRI_epiyear': 'epiyear',
                                     'DT_SIN_PRI_epiweek': 'epiweek', 'SinPri2Digita_DelayWeeks': 'delayweeks'})
    df_out.UF = df_out.UF.astype(int).astype(str)

    for i, ft in enumerate(['hospdeath', 'sragnofever', 'srag']):
        extract_quantile(df_out[df_out.filtro >= (i+1)], ft)

    del df_out

    # Impute digitalization date if needed:
    if dfappend is not None:
        dfappend = _cleanup(dfappend)
        df = dfappend.append(df, ignore_index=True, sort=False)

    opp_cols = list(set(tgt_cols).difference(['dado']).union(['sragflu', 'obitoflu', 'sragcovid', 'obitocovid',
                                                              'obito']))
    df = df[opp_cols].rename(columns={'DT_SIN_PRI_epiyearweek': 'epiyearweek', 'DT_SIN_PRI_epiyear': 'epiyear',
                                      'DT_SIN_PRI_epiweek': 'epiweek', 'SinPri2Digita_DelayWeeks': 'delayweeks'})
    df.UF = df.UF.astype(int).astype(str)
    df = delayimputation(df)

    df_out = wide2long(df)

    # Create tables and save to file:
    for k, suffix in filtro_dict.items():
        df_outnew = createtable(df_out[df_out.filtro >= k])
        for dataset in df_out.dado.unique():
            fout = '../clean_data/%s%s_sinpri2digita_table_weekly.csv' % (dataset, suffix)
            module_logger.info('Write table %s' % fout)
            df_outnew[df_outnew.dado == dataset].to_csv(fout, index=False)

    return


def main(flist, sep=',', yearmax=None, filtertype='srag', append_cases=None, append_delay=None):

    if filtertype not in ['srag', 'sragnofever', 'hospdeath']:
        module_logger.error('Invalid filter type: %s', filtertype)
        exit(1)

    suff = ''
    if filtertype != 'srag':
        suff = '_%s' % filtertype

    df = pd.DataFrame()
    for fname in flist:
        module_logger.info('Processing database file: %s', fname)
        dftmp = readtable(fname, sep)
        # Check if data file has 2019's database or not:
        if int(re.findall(r'\d+', fname)[0]) < 2019:
            module_logger.info('DB pre-2019')
            df = df.append(applysinanfilter(dftmp, tag=fname, filtertype=filtertype), ignore_index=True, sort=True)
        else:
            module_logger.info('DB 2019 onwards')
            df = df.append(filter_db2019(dftmp, tag=fname, filtertype=filtertype), ignore_index=True, sort=True)

    del dftmp

    if yearmax:
        df = df[(df.DT_SIN_PRI.apply(lambda x: x.year) <= yearmax)]

    df.NM_LOGRADO = clean_open_field(df.NM_LOGRADO)
    df.DS_PCR_OUT = clean_open_field(df.DS_PCR_OUT)
    df.DS_IF_OUT = clean_open_field(df.DS_IF_OUT)
    df.DS_AN_OUT = clean_open_field(df.DS_AN_OUT)
    df.OUTRO_DES = clean_open_field(df.OUTRO_DES)
    df.MORB_DESC = clean_open_field(df.MORB_DESC)
    df.RAIOX_OUT = clean_open_field(df.RAIOX_OUT)
    df.TOMO_OUT = clean_open_field(df.TOMO_OUT)

    # Convert date fields to text and leave NaT as empty cell
    regexp = re.compile('^DT')
    target_cols = list(filter(regexp.match, df.columns))
    df[target_cols] = df[target_cols].applymap(lambda x: str(x.date())).where(lambda x: x != 'NaT', np.nan)
    df = insert_epiweek(df)

    if 'ID_UNIDADE' in df.columns:
        df.CO_UNI_NOT.update(df.ID_UNIDADE)

    mask_sanity_check = (
            (df.SARS2 == 1) &
            (
                    (df.DT_SIN_PRI_epiyear.astype(int) < 2020) |
                    ((df.DT_SIN_PRI_epiyear.astype(int) == 2020) &
                     (df.DT_SIN_PRI_epiweek.astype(int) < 8))
            )
    )
    if sum((df.SARS2[mask_sanity_check])) > 1:
        print('ATENÇÃO: Caso de COVID-19 anterior a 2020 08. Entrar em contato com GT-Influenza')
        df.loc[mask_sanity_check, 'SARS2'] = 0

    if append_cases:
        tmp = readtable(append_cases)
        dfout = tmp.append(df, ignore_index=True)
    else:
        dfout = df.copy()

    mask_flu = ((pd.notnull(dfout.FLU_A) & (dfout.FLU_A == 1)) |
                (pd.notnull(dfout.FLU_B) & (dfout.FLU_B == 1)) |
                (dfout.FLU_CLINIC == 1))
    mask_covid = (dfout.SARS2 == 1)
    mask_obito = (dfout.EVOLUCAO == 2)
    mask_obitoflu = mask_flu & mask_obito
    mask_obitocovid = mask_covid & mask_obito

    for k, suffix in filtro_dict.items():
        dfout[dfout.filtro >= k].to_csv('../clean_data/clean_data_srag%s_epiweek.csv' % suffix, index=False)
        dfout[(dfout.filtro >= k) & mask_flu].to_csv('../clean_data/clean_data_sragflu%s_epiweek.csv' % suffix, index=False)
        dfout[(dfout.filtro >= k) & mask_obitoflu].to_csv('../clean_data/clean_data_obitoflu%s_epiweek.csv' % suffix,
                                                    index=False)
        dfout[(dfout.filtro >= k) & mask_covid].to_csv('../clean_data/clean_data_sragcovid%s_epiweek.csv' % suffix,
                                                 index=False)
        dfout[(dfout.filtro >= k) & mask_obitocovid].to_csv('../clean_data/clean_data_obitocovid%s_epiweek.csv' % suffix,
                                                      index=False)
        dfout[(dfout.filtro >= k) & mask_obito].to_csv('../clean_data/clean_data_obito%s_epiweek.csv' % suffix, index=False)
    del dfout

    def masks(dfin: pd.DataFrame):
        mask_flu = ((pd.notnull(dfin.FLU_A) & (dfin.FLU_A == 1)) |
                    (pd.notnull(dfin.FLU_B) & (dfin.FLU_B == 1)) |
                    (dfin.FLU_CLINIC == 1))
        mask_covid = (dfin.SARS2 == 1)
        mask_obito = (dfin.EVOLUCAO == 2)
        mask_obitoflu = mask_flu & mask_obito
        mask_obitocovid = mask_covid & mask_obito
        dfin['obito'] = mask_obito.astype(int)
        dfin['sragflu'] = mask_flu.astype(int)
        dfin['obitoflu'] = mask_obitoflu.astype(int)
        dfin['sragcovid'] = mask_covid.astype(int)
        dfin['obitocovid'] = mask_obitocovid.astype(int)

        return

    masks(df)
    if append_cases:
        masks(tmp)
        delays_dist(df, filtertype, append_delay, dfappend=tmp)
    else:
        delays_dist(df, filtertype)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Clean SINAN SRAG table.\n" +
                                                 "python3 sinan_clean.py --path ../data/influ*.csv --sep ,\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--path', nargs='*', action='append', help='Path to data file')
    parser.add_argument('--sep', help='Column separator', default=',')
    parser.add_argument('--year', help='Maximum year', default=None)
    parser.add_argument('--filtertype', help='Default=srag. Which filter should be used? [srag, sragnofever, '
                                             'hospdeath]', default='srag')
    args = parser.parse_args()
    print(args)
    main(args.path[0], args.sep, args.year, args.filtertype)
