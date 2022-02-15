# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'
import pandas as pd
import argparse
from argparse import RawDescriptionHelpFormatter

tabela_ufnome = {'11': 'Rondônia',
                 '12': 'Acre',
                 '13': 'Amazonas',
                 '14': 'Roraima',
                 '15': 'Pará',
                 '16': 'Amapá',
                 '17': 'Tocantins',
                 '21': 'Maranhão',
                 '22': 'Piauí',
                 '23': 'Ceará',
                 '24': 'Rio Grande do Norte',
                 '25': 'Paraíba',
                 '26': 'Pernambuco',
                 '27': 'Alagoas',
                 '28': 'Sergipe',
                 '29': 'Bahia',
                 '31': 'Minas Gerais',
                 '32': 'Espírito Santo',
                 '33': 'Rio de Janeiro',
                 '35': 'São Paulo',
                 '41': 'Paraná',
                 '42': 'Santa Catarina',
                 '43': 'Rio Grande do Sul',
                 '50': 'Mato Grosso do Sul',
                 '51': 'Mato Grosso',
                 '52': 'Goiás',
                 '53': 'Distrito Federal',
                 'RegN': 'Regional Norte',
                 'RegC': 'Regional Centro',
                 'RegL': 'Regional Leste',
                 'RegS': 'Regional Sul',
                 'BR': 'Brasil',
                 'S': 'Região Sul',
                 'N': 'Região Norte',
                 'CO': 'Região Centro-oeste',
                 'NE': 'Região Nordeste',
                 'SE': 'Região Sudeste'}


def create_brackets(fname):
    '''
    Create age brackets from IBGE predefined bracket projections
    :return:
    '''

    dfibge = pd.read_csv(fname, encoding='utf-8')
    age_cols = ['0-4 anos', '5-9 anos', '10-19 anos', '20-29 anos', '30-39 anos', '40-49 anos',
                '50-59 anos', '60+ anos']
    dfibge['60+ anos'] = dfibge[['60-64', '65-69', '70-74', '75-79', '80-84', '85-89',
                                 '90+']].apply(sum, axis=1)
    dfibge.rename(columns={'0-4': '0-4 anos', '5-9': '5-9 anos'}, inplace=True)
    dfibge['10-19 anos'] = dfibge[['10-14', '15-19']].apply(sum, axis=1)
    dfibge['20-29 anos'] = dfibge[['20-24', '25-29']].apply(sum, axis=1)
    dfibge['30-39 anos'] = dfibge[['30-34', '35-39']].apply(sum, axis=1)
    dfibge['40-49 anos'] = dfibge[['40-44', '45-49']].apply(sum, axis=1)
    dfibge['50-59 anos'] = dfibge[['50-54', '55-59']].apply(sum, axis=1)

    dfibge = dfibge[['Sigla', 'Ano', 'Sexo', 'Total']+age_cols].copy()

    dfreg = pd.read_csv('../data/regioesclimaticas.csv', encoding='utf-8')
    dfibge = dfibge.merge(dfreg)

    dfibge_regs = dfibge.groupby(['Região', 'Ano', 'Sexo'], as_index=False).sum()[['Região', 'Ano', 'Sexo',
                                                                                   'Total'] + age_cols]
    dfibge_regs.rename(columns={'Região': 'Código'}, inplace=True)
    dfibge_regs['UF'] = dfibge_regs['Código'].map(tabela_ufnome)

    dfibge_regs_ofi = dfibge.groupby(['Região oficial', 'Ano', 'Sexo'], as_index=False).sum()[['Região oficial',
                                                                                               'Ano', 'Sexo',
                                                                                   'Total']+age_cols]
    dfibge_regs_ofi.rename(columns={'Região oficial': 'Código'}, inplace=True)
    dfibge_regs_ofi['UF'] = dfibge_regs_ofi['Código'].map(tabela_ufnome)
    dfibge_regs_ofi = dfibge_regs_ofi[~(dfibge_regs_ofi['Código'] == 'BR')]

    dfibge = dfibge[~(dfibge['Código'] == 0)]
    dfibge = pd.concat([dfibge, dfibge_regs], sort=True)
    dfibge = pd.concat([dfibge, dfibge_regs_ofi], sort=True)

    dfibge = dfibge[['Código', 'Sigla', 'UF', 'Região', 'Região oficial', 'Ano', 'Sexo', 'Total']+age_cols].copy()
    dfibge = dfibge.sort_values(by=['Código', 'Ano', 'Sexo'], axis=0).reset_index().drop('index', axis=1)

    fout = fname[:-4]+'_agebracket.csv'
    dfibge.to_csv(fout, index=False, encoding='utf-8')

    return


def main(fname):

    create_brackets(fname)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Generate appropriate age brackets.\n" +
                                     "python3 anual_age_brackets.py --path "
                                     "../data/PROJECOES_2013_POPULACAO-simples_v3.csv --sep ,\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('--path', help='Path to data file', default='../data/PROJECOES_2013_POPULACAO-simples.csv')
    args = parser.parse_args()
    main(args.path)
