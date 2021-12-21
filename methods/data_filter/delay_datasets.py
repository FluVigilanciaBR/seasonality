__author__ = 'Marcelo Ferreira da Costa Gomes'

import pandas as pd

import logging

module_logger = logging.getLogger('update_system.delay_datasets')


def readtable(fname):
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
                'Notific2Encerra_DelayWeeks',
                'Coleta2IFI_DelayWeeks',
                'Coleta2PCR_DelayWeeks',
                'Notific2Coleta_DelayWeeks',
                'Notific2Antivir_DelayWeeks',
                'Digita2Antivir_DelayWeeks',

                'Notific2Digita_DelayDays',
                'SinPri2Digita_DelayDays',
                'SinPri2Antivir_DelayDays',
                'SinPri2Notific_DelayDays',
                'SinPri2Coleta_DelayDays',
                'Notific2Encerra_DelayDays',
                'Coleta2IFI_DelayDays',
                'Coleta2PCR_DelayDays',
                'Notific2Coleta_DelayDays',
                'Notific2Antivir_DelayDays',
                'Digita2Antivir_DelayDays',
                ]
    df = pd.read_csv(fname, low_memory=False)[tgt_cols].rename(columns={'DT_NOTIFIC_epiyear': 'epiyear',
                                                                        'DT_NOTIFIC_epiweek': 'epiweek',
                                                                        'SG_UF_NOT': 'UF'})

    return(df)


def main(filtertype='srag'):

    if filtertype not in ['srag', 'sragnofever', 'hospdeath']:
        exit('Invalid filter type: %s' % filtertype)

    suff = ''
    if filtertype != 'srag':
        suff = '_%s' % filtertype

    dataset = ['srag', 'sragflu', 'obitoflu', 'sragcovid', 'obitocovid', 'obito']
    dataset_name = dataset[0]
    module_logger.info('Processing dataset: %s', dataset_name)
    df = readtable('../clean_data/clean_data_%s%s_epiweek.csv.gz' % (dataset_name, suff))
    df['dado'] = dataset_name
    module_logger.info('... DONE')
    for dataset_name in dataset[1:]:
        module_logger.info('Processing dataset: %s', dataset_name)
        dftmp = readtable('../clean_data/clean_data_%s%s_epiweek.csv.gz' % (dataset_name, suff))
        dftmp['dado'] = dataset_name
        df = df.append(dftmp, sort=True)
        module_logger.info('... DONE')

    df.loc[pd.isnull(df.UF), 'UF'] = 99
    df.UF = df.UF.astype(int)
    # Insert region info:
    df_reg = pd.read_csv('../data/regioesclimaticas.csv', low_memory=False)[['Código', 'Região', 'Região oficial']]
    df_reg.rename(columns={'Código': 'UF', 'Região': 'Regional', 'Região oficial': 'Regiao'}, inplace=True)
    df = df.merge(df_reg, on='UF')
    df['Pais'] = 'BR'

    df.to_csv('../../data/data/delay_table%s.csv.gz' % suff, index=False)


if __name__ == '__main__':
    main()
