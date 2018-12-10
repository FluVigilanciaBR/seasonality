# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

# Apply filters of interest in a dataframe containing SINAN-SRAG data
import pandas as pd
import numpy as np
import argparse
import logging
from argparse import RawDescriptionHelpFormatter


module_logger = logging.getLogger('update_system.sinan_filter_of_interest')


def readtable(fname, sep):
    try:
        df = pd.read_csv(fname, sep=sep, low_memory=False, encoding='utf-8')
    except:
        df = pd.read_csv(fname, sep=sep, low_memory=False, encoding='utf-16')

    return (df)


def applysinanfilter(df, tag=None):
    # Filter columns of interest
    # Na solicitação, além das variáveis abaixo, necessitamos também do ID do caso
    tgtcols = ['SEM_NOT', 'DT_NOTIFIC', 'SG_UF_NOT', 'DT_INTERNA', 'DT_SIN_PRI', 'DT_DIGITA', 'HOSPITAL',
               'FEBRE', 'CLASSI_FIN', 'CRITERIO', 'SG_UF', 'ID_MN_RESI', 'ID_RG_RESI', 'SEM_PRI',
               'TOSSE', 'GARGANTA', 'DISPNEIA', 'SATURACAO', 'DESC_RESP', 'EVOLUCAO', 'DT_COLETA', 'IFI', 'DT_IFI',
               'PCR', 'DT_PCR_1', 'OUT_METODO', 'DS_OUTMET', 'DT_OUTMET', 'RES_FLUA', 'RES_FLUASU', 'RES_FLUB',
               'RES_VSR', 'RES_PARA1', 'RES_PARA2', 'RES_PARA3', 'RES_ADNO', 'RES_OUTRO', 'DT_PCR', 'PCR_RES',
               'PCR_ETIOL', 'PCR_TIPO_H', 'PCR_TIPO_N', 'DT_CULTURA', 'CULT_RES', 'DT_HEMAGLU', 'HEMA_RES',
               'HEMA_ETIOL', 'HEM_TIPO_H', 'HEM_TIPO_N', 'VACINA', 'DT_UT_DOSE', 'ANT_PNEUMO', 'DT_PNEUM',
               'CO_UF_INTE', 'CO_MU_INTE', 'CO_UN_INTE', 'DT_ENCERRA', 'NU_NOTIFIC', 'ID_AGRAVO', 'ID_MUNICIP',
               'ID_REGIONA', 'ID_UNIDADE', 'NU_IDADE_N', 'CS_SEXO', 'CS_GESTANT', 'CS_RACA', 'DT_ANTIVIR', 'DT_OBITO']

    cols = df.columns
    if 'RES_VRS' in cols:
        df.rename(columns={'RES_VRS': 'RES_VSR'}, inplace=True)

    for col in set(tgtcols).difference(cols):
        df[col] = None

    df = df[tgtcols].copy()
    dt_cols = ['DT_NOTIFIC', 'DT_INTERNA', 'DT_SIN_PRI', 'DT_DIGITA', 'DT_COLETA', 'DT_IFI', 'DT_PCR_1', 'DT_OUTMET',
               'DT_PCR', 'DT_CULTURA', 'DT_HEMAGLU', 'DT_UT_DOSE', 'DT_PNEUM', 'DT_ENCERRA', 'DT_ANTIVIR', 'DT_OBITO']


    # Filter by notification date
    df = df.where(df != -1, None)
    df[dt_cols] = df[dt_cols].where(df[dt_cols] != 10101, None)
    if df.CRITERIO.dtype == 'O':
        df.CRITERIO = df.CRITERIO.where(df.CRITERIO != 'NÃ', None)
    df.dropna(subset=["DT_SIN_PRI", "DT_NOTIFIC"], inplace=True)

    # Filter by symptoms:
    # All cases, regardless of year, must either attend symptoms definition or have evolved to
    # death. Besides that, cases from 2009 do not need hospitalization to be considered as true case, per Ministry
    # of Health request. For all other years, they must have been hospitalized or have evolved to death.
    # The filter regarding hospitalization and case evolution is done after date columns consolidation to attend
    # 2009's particularity.
    df = df[(
                (df.FEBRE == 1) &
                ((df.TOSSE == 1) | (df.GARGANTA == 1)) &
                ((df.DISPNEIA == 1) | (df.SATURACAO == 1) | (df.DESC_RESP == 1))
            ) |
            (df.EVOLUCAO == 2)].copy()

    # Convert all date related columns to datetime format
    cols = df.columns
    # Check date input format
    sample = df.DT_NOTIFIC.iloc[0]
    dtformat = '%Y%m%d'
    if isinstance(sample, str):
        dtsep = '-'
        if '/' in sample:
            dtsep = '/'
        dttest = pd.DataFrame(list(df.DT_NOTIFIC.str.split(dtsep)))
        maxvals = [int(dttest[i].max()) for i in range(3)]
        del dttest
        yearpos = maxvals.index(max(maxvals))
        if yearpos == 2:
            dtformat = '%d' + dtsep + '%m' + dtsep + '%Y'
        else:
            dtformat = '%Y' + dtsep + '%m' + dtsep + '%d'

    for col in cols:
        if 'DT' in col:
            # Convert all date columns to datetime format. Output will have the format YYYY-MM-DD
            df[col] = pd.to_datetime(df[col], errors='coerce', format=dtformat)

    # Discard those neither hospitalized nor deceased. For cases from 2009, keep all:
    df = df[(df.DT_SIN_PRI.apply(lambda x: x.year) == 2009) | (df.HOSPITAL == 1) | (df.EVOLUCAO == 2)]

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
    df['ADNO'] = None

    df['OTHERS'] = None
    df['NEGATIVE'] = None
    df['INCONCLUSIVE'] = None
    df['DELAYED'] = None

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
        # 1010 : 10 days
        # 2010: 10 months
        # 3010: 10 years
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

    return (df)


def main(flist, sep=',', yearmax=None):
    df = pd.DataFrame()
    for fname in flist:
        module_logger.info('Processing database file: %s', fname)
        dftmp = readtable(fname, sep)
        df = df.append(applysinanfilter(dftmp, tag=fname), ignore_index=True, sort=True)

    if (yearmax):
        df = df[(df.DT_SIN_PRI.apply(lambda x: x.year) <= yearmax)]

    df.to_csv('clean_data_srag.csv', index=False)
    dfflu = df[(pd.notnull(df.FLU_A) & (df.FLU_A == 1)) | (pd.notnull(df.FLU_B) & (df.FLU_B == 1)) | (df.FLU_CLINIC
                                                                                                      == 1)]
    dfflu.to_csv('clean_data_sragflu.csv', index=False)
    dffluobito = dfflu[dfflu.EVOLUCAO == 2]
    dffluobito.to_csv('clean_data_obitoflu.csv', index=False)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Clean SINAN SRAG table.\n" +
                                                 "python3 sinan_clean.py --path ../data/influ*.csv --sep ,\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--path', nargs='*', action='append', help='Path to data file')
    parser.add_argument('--sep', help='Column separator', default=',')
    parser.add_argument('--year', help='Maximum year', default=None)
    args = parser.parse_args()
    print(args)
    main(args.path[0], args.sep, args.year)
