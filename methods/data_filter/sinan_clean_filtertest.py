#coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'
import pandas as pd
import argparse
from argparse import RawDescriptionHelpFormatter

def readtable(fname, sep=','):

    """
    Read csv file and return cleaned dataframe, checking for SRAG definition and date inconsistencies.
    :param fname: path to file
    :param sep: column separator. Default=','
    :return df: Cleaned dataframe
    """
    tgtcols = ['SEM_NOT', 'DT_NOTIFIC', 'SG_UF_NOT', 'DT_INTERNA', 'DT_SIN_PRI', 'SRAG2012', 'DT_DIGITA',
               'FEBRE', 'TOSSE', 'GARGANTA', 'DISPNEIA', 'SATURACAO', 'DESC_RESP', 'EVOLUCAO',
               'DT_COLETA', 'IFI', 'DT_IFI', 'PCR', 'OUT_METODO', 'DS_OUTMET', 'DT_OUTMET', 'RES_FLUA',
               'RES_FLUASU', 'RES_FLUB', 'RES_VSR', 'RES_PARA1', 'RES_PARA2', 'RES_PARA3', 'RES_ADNO', 'RES_OUTRO',
               'DT_PCR', 'PCR_RES', 'PCR_ETIOL', 'PCR_TIPO_H', 'PCR_TIPO_N', 'DT_CULTURA', 'CULT_RES', 'DT_HEMAGLU',
               'HEMA_RES', 'HEMA_ETIOL', 'HEM_TIPO_H', 'HEM_TIPO_N']

    """
    ### OBSERVAÇÃO ###
    A ficha mudou em 2012.
    Durante os anos 2009-2011 era uma, a partir de 2012 passou a ser outra.
    As variáveis
    'DT_COLETA', 'IFI', 'DT_IFI', 'PCR', 'OUT_METODO', 'DS_OUTMET', 'DT_OUTMET', 'RES_FLUA',
    'RES_FLUASU', 'RES_FLUB', 'RES_VRS', 'RES_PARA1', 'RES_PARA2', 'RES_PARA3', 'RES_ADNO', 'RES_OUTRO'
    são da ficha nova (2012-), já as variáveis
    'DT_PCR', 'PCR_RES', 'PCR_ETIOL', 'PCR_TIPO_H', 'PCR_TIPO_N', 'DT_CULTURA', 'CULT_RES', 'DT_HEMAGLU',
    'HEMA_RES', 'HEMA_ETIOL', 'HEM_TIPO_H', 'HEM_TIPO_N'
    são da ficha antiga (2009-2011).
    As colunas correspondentes a todas estas variáveis está presente nos csv independentemente do ano correspondente.
    Porém, estarão preenchidas apenas para os anos em questão. Em 2012 houve transição das fichas, sendo que alguns
    registros foram catalogados com a ficha antiga, outros com a ficha nova, de modo que há entradas em que as colunas
    antigas estão preenchidas e outras em que apenas as colunas da ficha nova estão.
    Ver arquivos DIC_DADOS_Ficha_[Nova,Antiga].pdf para detalhes.
    """

    # Read csv file, keeping only relevant columns:
    df = pd.read_csv(fname, sep=sep)[tgtcols]
    # Drop rows without notification week and State:
    df.dropna(subset=['SEM_NOT', 'SG_UF_NOT', 'DT_NOTIFIC'], inplace=True)
    # Keep only those that abide to current definition of SRAG:
    df = df[( (df.FEBRE == 1) & ( (df.TOSSE == 1) | (df.GARGANTA == 1) ) & ( (df.DISPNEIA == 1) | (df.SATURACAO == 1) |
                                                                             (df.DESC_RESP == 1) ) ) | (df.EVOLUCAO ==
                                                                                                       2) ]

    # Ensure that field related to first symptoms has slashes (minimum requirement for valid date entry)
    df = df[df.DT_NOTIFIC.str.contains('-')]

    cols = df.columns
    for col in cols:
        if 'DT' in col:
            # Convert all date columns to datetime format. Output will have the format YYYY-MM-DD
            # PS: assumes original file format is YYYY/MM/DD
            df[col] = pd.to_datetime(df[col], errors='coerce', coerce=True, format='%Y-%m-%d')

    # Discard inconsistent rows:
    #df = df[((df.DT_INTERNA - df.DT_SIN_PRI)/pd.offsets.Day(1) > 0) &
    #        ((df.DT_INTERNA - df.DT_SIN_PRI)/pd.offsets.Day(1) < 20)]


    return df


def main(flist, sep=','):
    print(flist)
    print(flist[0])
    df = readtable(flist[0], sep=sep)
    for fname in flist[1:]:
        print(fname)
        df = df.append(readtable(fname, sep), ignore_index=True)

    df.to_csv('clean_data_filtro_sintomas.csv', index=False)


if __name__ == '__main__':

    parser = argparse.ArgumentParser(description="Clean SINAN SRAG table.\n" +
                                     "python3 sinan_clean.py --path data/influ*.csv --sep ,\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--path', nargs='*', action='append', help='Path to data file')
    parser.add_argument('--sep', help='Column separator', default=',')
    args = parser.parse_args()
    print(args)
    main(args.path[0], args.sep)
