from .settings import DATABASE, PATH

import glob
import os
import pandas as pd
import sqlalchemy as sqla
import argparse
from argparse import RawDescriptionHelpFormatter

dataset_id = {
    'srag': 1,
    'sragflu': 2,
    'obitoflu': 3,
    'sragcovid': 4,
    'obitocovid': 5,
    'obito': 6
}

# ### 1.2 Scale

scale_id = {
    'incidência': 1,
    'casos': 2
}

# ### 1.3 Situation

situation_id = {
    'unknown': 1,
    'estimated': 2,
    'stable': 3,
    'incomplete': 4
}

# ### 1.4 Territory Type

territory_type_id = {
    'Estado': 1,
    'Regional': 2,
    'Região': 3,
    'País': 4
}

# ### 1.5 Region id conversion


region_id = {
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
    'RNI': 9
}

# ### Territory Table

df_territory = pd.DataFrame([
    {'id': 11, 'initials': 'RO', 'name': 'Rondônia',
     'territory_type_id': 1},
    {'id': 12, 'initials': 'AC', 'name': 'Acre', 'territory_type_id': 1},
    {'id': 13, 'initials': 'AM', 'name': 'Amazonas',
     'territory_type_id': 1},
    {'id': 14, 'initials': 'RR', 'name': 'Roraima',
     'territory_type_id': 1},
    {'id': 15, 'initials': 'PA', 'name': 'Pará', 'territory_type_id': 1},
    {'id': 16, 'initials': 'AP', 'name': 'Amapá', 'territory_type_id': 1},
    {'id': 17, 'initials': 'TO', 'name': 'Tocantins',
     'territory_type_id': 1},
    {'id': 21, 'initials': 'MA', 'name': 'Maranhão',
     'territory_type_id': 1},
    {'id': 22, 'initials': 'PI', 'name': 'Piauí', 'territory_type_id': 1},
    {'id': 23, 'initials': 'CE', 'name': 'Ceará', 'territory_type_id': 1},
    {'id': 24, 'initials': 'RN', 'name': 'Rio Grande do Norte',
     'territory_type_id': 1},
    {'id': 25, 'initials': 'PB', 'name': 'Paraíba',
     'territory_type_id': 1},
    {'id': 26, 'initials': 'PE', 'name': 'Pernambuco',
     'territory_type_id': 1},
    {'id': 27, 'initials': 'AL', 'name': 'Alagoas',
     'territory_type_id': 1},
    {'id': 28, 'initials': 'SE', 'name': 'Sergipe',
     'territory_type_id': 1},
    {'id': 29, 'initials': 'BA', 'name': 'Bahia', 'territory_type_id': 1},
    {'id': 31, 'initials': 'MG', 'name': 'Minas Gerais',
     'territory_type_id': 1},
    {'id': 32, 'initials': 'ES', 'name': 'Espírito Santo',
     'territory_type_id': 1},
    {'id': 33, 'initials': 'RJ', 'name': 'Rio de Janeiro',
     'territory_type_id': 1},
    {'id': 35, 'initials': 'SP', 'name': 'São Paulo',
     'territory_type_id': 1},
    {'id': 41, 'initials': 'PR', 'name': 'Paraná', 'territory_type_id': 1},
    {'id': 42, 'initials': 'SC', 'name': 'Santa Catarina',
     'territory_type_id': 1},
    {'id': 43, 'initials': 'RS', 'name': 'Rio Grande do Sul',
     'territory_type_id': 1},
    {'id': 50, 'initials': 'MS', 'name': 'Mato Grosso do Sul',
     'territory_type_id': 1},
    {'id': 51, 'initials': 'MT', 'name': 'Mato Grosso',
     'territory_type_id': 1},
    {'id': 52, 'initials': 'GO', 'name': 'Goiás', 'territory_type_id': 1},
    {'id': 53, 'initials': 'DF', 'name': 'Distrito Federal',
     'territory_type_id': 1},
    {'id': 99, 'initials': 'NI', 'name': 'Não informado',
     'territory_type_id': 1},
    {'id': 0, 'initials': 'BR', 'name': 'Brasil', 'territory_type_id': 4},
    {'id': 1003, 'initials': 'RegC', 'name': 'Regional Centro',
     'territory_type_id': 2},
    {'id': 1002, 'initials': 'RegL', 'name': 'Regional Leste',
     'territory_type_id': 2},
    {'id': 1001, 'initials': 'RegN', 'name': 'Regional Norte',
     'territory_type_id': 2},
    {'id': 1004, 'initials': 'RegS', 'name': 'Regional Sul',
     'territory_type_id': 2},
    {'id': 9999, 'initials': 'RegSNI', 'name': 'Regional não informada',
     'territory_type_id': 2},
    {'id': 1, 'initials': 'N', 'name': 'Norte', 'territory_type_id': 3},
    {'id': 2, 'initials': 'NE', 'name': 'Nordeste',
     'territory_type_id': 3},
    {'id': 3, 'initials': 'SE', 'name': 'Sudeste', 'territory_type_id': 3},
    {'id': 5, 'initials': 'CO', 'name': 'Centro-oeste',
     'territory_type_id': 3},
    {'id': 4, 'initials': 'S', 'name': 'Sul', 'territory_type_id': 3},
    {'id': 9, 'initials': 'RNI', 'name': 'Região não informada',
     'territory_type_id': 3},
])


contingency_name_from_id = {
    0: 'Nível basal',
    1: 'Nível 0',
    2: 'Nível 1',
    3: 'Nível 2',
}


def update_data_files(force: bool):
    path_data = os.path.join(PATH, '../../data/data')

    update_params = '-nc' if not force else '-N'
    wget_prefix = (
        ('wget %s ' % update_params) +
        'https://raw.githubusercontent.com/FluVigilanciaBR/data/master/data'
    )

    command = '''cd %(path_data)s; \
    %(wget_prefix)s/br-states.json; \
    %(wget_prefix)s/clean_data_epiweek-weekly-incidence_w_situation.csv && \
    %(wget_prefix)s/contingency_level.csv && \
    %(wget_prefix)s/current_estimated_values.csv && \
    %(wget_prefix)s/historical_estimated_values.csv && \
    %(wget_prefix)s/mem-report.csv && \
    %(wget_prefix)s/mem-typical.csv && \
    %(wget_prefix)s/season_level.csv && \
    %(wget_prefix)s/weekly_alert.csv && \
    %(wget_prefix)s/delay_table.csv''' % {
        'path_data': path_data,
        'wget_prefix': wget_prefix
    }
    print(command.replace('&&', ' && \ \n'))
    os.system(command)
    print('[II] DONE!')


def get_filename_from_path(file_path: str):
    """
    """
    return file_path.split(os.path.sep)[-1].split('.')[0]


def migrate_current_estimates(df):

    migration_rules = {
        'UF': 'territory_id',
        'SRAG': 'value',
        'Tipo': 'territory_type',  # Not needed in the table
        'Situation': 'situation_id',
        '50%': 'median',
        '2.5%': 'ci_lower',
        '97.5%': 'ci_upper',
        'bounded_97.5%': 'ci_upper_bounded',
        'cntry_percentage': 'country_percentage',
        'L0': 'low_level',
        'L1': 'epidemic_level',
        'L2': 'high_level',
        'L3': 'very_high_level',
        'Run date': 'run_date',
        'dado': 'dataset_id',
        'escala': 'scale_id'
    }

    # rename columns
    df.rename(
        columns=migration_rules, inplace=True
    )

    # apply categories
    df.dataset_id = df.dataset_id.map(dataset_id)
    df.scale_id = df.scale_id.map(scale_id)
    df.situation_id = df.situation_id.map(situation_id)
    regions_indeces = df.territory_id.isin([
        'BR', 'RegN', 'RegL', 'RegC', 'RegS', 'RegNI',
        'N', 'NE', 'SE', 'S', 'CO', 'RNI'
    ])
    df.loc[regions_indeces, 'territory_id'] = df.loc[
        regions_indeces, 'territory_id'
    ].map(region_id)
    df.territory_id = df.territory_id.astype(int)

    # remove unnecessary fields
    df.drop(['territory_type', '25%', '75%', '5%', '95%'], axis=1, inplace=True)

    # primary_keys
    pks = ['dataset_id', 'scale_id', 'territory_id', 'epiyear',
                    'epiweek']

    df.set_index(pks, inplace=True)

    return df


def migrate_historical_estimates(df):
    
    migration_rules = {
        'UF': 'territory_id',
        'SRAG': 'value',
        'Tipo': 'territory_type',  # Not needed in the table
        'Situation': 'situation_id',
        '50%': 'median',
        '2.5%': 'ci_lower',
        '97.5%': 'ci_upper',
        'bounded_97.5%': 'ci_upper_bounded',
        'cntry_percentage': 'country_percentage',
        'L0': 'low_level',
        'L1': 'epidemic_level',
        'L2': 'high_level',
        'L3': 'very_high_level',
        'Run date': 'run_date',
        'dado': 'dataset_id',  # or origin
        'escala': 'scale_id'
    }

    df.rename(
        columns=migration_rules, inplace=True
    )
    # apply categories
    df.dataset_id = df.dataset_id.map(dataset_id)
    df.scale_id = df.scale_id.map(scale_id)
    df.situation_id = df.situation_id.map(situation_id)
    regions_indeces = df.territory_id.isin([
        'BR', 'RegN', 'RegL', 'RegC', 'RegS', 'RegNI',
        'N', 'NE', 'SE', 'S', 'CO', 'RNI'
    ])
    df.loc[regions_indeces, 'territory_id'] = df.loc[
        regions_indeces, 'territory_id'
    ].map(region_id)
    df.territory_id = df.territory_id.astype(int)

    # remove unnecessary fields
    df.drop(['territory_type', '25%', '75%', '5%', '95%'], axis=1, inplace=True)

    # primary_keys
    pks = [
        'dataset_id', 'scale_id', 'territory_id',
        'base_epiyear', 'base_epiweek',
        'epiyear', 'epiweek'
    ]

    df.set_index(pks, inplace=True)
    df.head()

    return df


def migrate_clean_data_epiweek(df):

    migration_rules = {
        '0-4 anos': 'years_0_4',
        '10-19 anos': 'years_10_19',
        '2-4 anos': 'years_2_4',
        '20-29 anos': 'years_20_29',
        '30-39 anos': 'years_30_39',
        '40-49 anos': 'years_40_49',
        '5-9 anos': 'years_5_9',
        '50-59 anos': 'years_50_59',
        '60+ anos': 'years_60_or_more',
        '< 2 anos': 'years_lt_2',
        'DELAYED': 'delayed',
        'FLU_A': 'flu_a',
        'FLU_B': 'flu_b',
        'INCONCLUSIVE': 'inconclusive',
        'Idade desconhecida': 'unknown_age',
        'NEGATIVE': 'negative',
        'NOTTESTED': 'not_tested',
        'OTHERS': 'others',
        'POSITIVE_CASES': 'positive_cases',
        'SRAG': 'value',
        'Situation': 'situation_id',
        'TESTING_IGNORED': 'testing_ignored',
        'Tipo': 'territory_type',  # Not needed in the table
        'UF': 'territory_id',
        'Unidade da Federação': 'state_country_name',  # Not needed
        'VSR': 'vsr',
        'dado': 'dataset_id',
        'escala': 'scale_id',
        'sexo': 'gender'
    }

    df.rename(
        columns=migration_rules, inplace=True
    )
    # apply categories
    df.dataset_id = df.dataset_id.map(dataset_id)
    df.scale_id = df.scale_id.map(scale_id)
    df.situation_id = df.situation_id.map(situation_id)
    regions_indeces = df.territory_id.isin([
        'BR', 'RegN', 'RegL', 'RegC', 'RegS', 'RegNI',
        'N', 'NE', 'SE', 'S', 'CO', 'RNI'
    ])
    df.loc[regions_indeces, 'territory_id'] = df.loc[
        regions_indeces, 'territory_id'
    ].map(region_id)
    df.territory_id = df.territory_id.astype(int)

    # remove unnecessary fields
    df.drop([
        'state_country_name', 'territory_type'
    ], axis=1, inplace=True)

    # primary_keys
    pks = ['dataset_id', 'scale_id', 'territory_id', 'epiyear',
                    'epiweek']

    df.set_index(pks, inplace=True)

    return df


def migrate_mem_report(df):

    migration_rules = {
        'UF': 'territory_id',
        'População': 'population',
        'Média geométrica do pico de infecção das temporadas regulares':
            'geom_average_peak',
        'região de baixa atividade típica': 'low_activity_region',
        'limiar pré-epidêmico': 'pre_epidemic_threshold',
        'intensidade alta': 'high_threshold',
        'intensidade muito alta': 'very_high_threshold',
        'SE típica do início do surto': 'epi_start',
        'SE típica do início do surto - IC inferior (2,5%)':
            'epi_start_ci_lower',
        'SE típica do início do surto - IC superior (97,5%)':
            'epi_start_ci_upper',
        'duração típica do surto': 'epi_duration',
        'duração típica do surto - IC inferior (2,5%)':
            'epi_duration_ci_lower',
        'duração típica do surto - IC superior (97,5%)':
            'epi_duration_ci_upper',
        'temporadas utilizadas para os corredores endêmicos':
            'regular_seasons',
        'ano': 'year',
        'Unidade da Federação': 'state_country_name',  # Not needed ...
        'Tipo': 'territory_type',  # Not needed in the table
        'dado': 'dataset_id',
        'escala': 'scale_id'
    }

    df.rename(
        columns=migration_rules, inplace=True
    )
    # apply categories
    df.dataset_id = df.dataset_id.map(dataset_id)
    df.scale_id = df.scale_id.map(scale_id)
    regions_indeces = df.territory_id.isin([
        'BR', 'RegN', 'RegL', 'RegC', 'RegS', 'RegNI',
        'N', 'NE', 'SE', 'S', 'CO', 'RNI'
    ])
    df.loc[regions_indeces, 'territory_id'] = df.loc[
        regions_indeces, 'territory_id'
    ].map(region_id)
    df.territory_id = df.territory_id.astype(int)

    # remove unnecessary fields
    df.drop([
        'state_country_name', 'territory_type'
    ], axis=1, inplace=True)

    # primary_keys
    pks = ['dataset_id', 'scale_id', 'territory_id', 'year']

    df.set_index(pks, inplace=True)

    return df


def migrate_mem_typical(df):
    
    migration_rules = {
        'UF': 'territory_id',
        'População': 'population',
        'corredor baixo': 'low',
        'corredor mediano': 'median',
        'corredor alto': 'high',
        'ano': 'year',
        'Unidade da Federação': 'state_country_name',  # Not needed ...
        'Tipo': 'territory_type',  # Not needed in the table
        'dado': 'dataset_id',
        'escala': 'scale_id'
    }

    df.rename(
        columns=migration_rules, inplace=True
    )
    # apply categories
    df.dataset_id = df.dataset_id.map(dataset_id)
    df.scale_id = df.scale_id.map(scale_id)
    regions_indeces = df.territory_id.isin([
        'BR', 'RegN', 'RegL', 'RegC', 'RegS', 'RegNI',
        'N', 'NE', 'SE', 'S', 'CO', 'RNI'
    ])
    df.loc[regions_indeces, 'territory_id'] = df.loc[
        regions_indeces, 'territory_id'
    ].map(region_id)
    df.territory_id = df.territory_id.astype(int)

    # remove unnecessary fields
    df.drop([
        'state_country_name', 'territory_type'
    ], axis=1, inplace=True)

    # primary_keys
    pks = ['dataset_id', 'scale_id', 'territory_id', 'year',
                    'epiweek']

    df.set_index(pks, inplace=True)
    
    return df


def migrate_delay_table(df):

    migration_rules = {
        'UF': 'territory_id',
        'SinPri2Interna_DelayDays': 'symptoms2hospitalization',
        'Interna2Evoluca_DelayDays': 'hospitalization2evolution',
        'Notific2Digita_DelayDays': 'notification2digitalization',
        'SinPri2Digita_DelayDays': 'symptoms2digitalization',
        'SinPri2Antivir_DelayDays': 'symptoms2antiviral',
        'SinPri2Notific_DelayDays': 'symptoms2notification',
        'SinPri2Coleta_DelayDays': 'symptoms2sample',
        'Notific2Encerra_DelayDays': 'notification2closure',
        'Coleta2IFI_DelayDays': 'sample2ifi',
        'Coleta2PCR_DelayDays': 'sample2pcr',
        'Regional': 'regional',
        'Regiao': 'region',
        'dado': 'dataset_id'
    }

    df.rename(columns=migration_rules, inplace=True)

    # apply categories
    df.dataset_id = df.dataset_id.map(dataset_id)
    df.territory_id = df.territory_id.astype(int)
    df.regional = df.regional.map(region_id).astype(int)
    df.region = df.region.map(region_id).astype(int)

    # remove unnecessary fields
    df.drop([
        'DT_SIN_PRI_epiyearweek',
        'DT_SIN_PRI_epiweek',
        'DT_SIN_PRI_epiyear',
        'DT_DIGITA_epiyearweek',
        'DT_DIGITA_epiyear',
        'DT_DIGITA_epiweek',
        'Notific2Digita_DelayWeeks',
        'SinPri2Digita_DelayWeeks',
        'SinPri2Antivir_DelayWeeks',
        'SinPri2Notific_DelayWeeks',
        'SinPri2Coleta_DelayWeeks',
        'SinPri2Interna_DelayWeeks',
        'Interna2Evoluca_DelayWeeks',
        'Notific2Encerra_DelayWeeks',
        'Coleta2IFI_DelayWeeks',
        'Coleta2PCR_DelayWeeks',
        'Notific2Coleta_DelayWeeks',
        'Notific2Antivir_DelayWeeks',
        'Digita2Antivir_DelayWeeks',
        'Notific2Coleta_DelayDays',
        'Notific2Antivir_DelayDays',
        'Digita2Antivir_DelayDays',
    ], axis=1, inplace=True)

    # add index
    df['id'] = df.index

    # primary keys
    pks = ['id', 'territory_id', 'epiyear', 'epiweek']
    df.set_index(pks, inplace=True)

    return df


def migrate_contingency_level(df):

    pks = ['territory_id', 'epiyear']
    df.set_index(pks, inplace=True)

    return df


def migrate_weekly_alert(df):

    pks = ['dataset_id', 'territory_id', 'epiyear', 'epiweek']
    df.set_index(pks, inplace=True)

    return df


def migrate_season_level(df):

    pks = ['dataset_id', 'territory_id', 'epiyear']
    df.set_index(pks, inplace=True)

    return df


def migrate_from_csv_to_psql(dfs=None, suff='', basic_tables=True):
    """

    :return:
    """

    if dfs is None:
        print('Data files:')
        dfs = {}
        path_data_files = os.path.join(PATH, '../../data/data', '*.csv')
        for file_path in glob.glob(path_data_files):
            filename = get_filename_from_path(file_path)

            print(filename)

            dfs[filename] = pd.read_csv(file_path)

    datasets_migration = {
        'current_estimated_values': migrate_current_estimates,
        'historical_estimated_values': migrate_historical_estimates,
        'clean_data_epiweek-weekly-incidence_w_situation': migrate_clean_data_epiweek,
        'mem-report': migrate_mem_report,
        'mem-typical': migrate_mem_typical,
        'delay_table': migrate_delay_table,
        'contingency_level': migrate_contingency_level,
        'weekly_alert': migrate_weekly_alert,
        'season_level': migrate_season_level
    }

    for k, df in dfs.items():
        print('Polishing table: %s' % k)
        dfs[k] = datasets_migration[k.replace(suff, '')](dfs[k])
        print('Done!')

    # ## 1. Setting IDs
    # ### 1.1 Datasets

    if basic_tables:
        # creating dataset dataframe
        df_dataset = pd.DataFrame({
            'id': list(dataset_id.values()),
            'name': list(dataset_id.keys())
        }).set_index('id')

        dfs['dataset'] = df_dataset

        # creating situation dataframe
        df_situation = pd.DataFrame({
            'id': list(situation_id.values()),
            'name': list(situation_id.keys())
        }).set_index('id')

        dfs['situation'] = df_situation

        # creating scale dataframe
        df_scale = pd.DataFrame({
            'id': list(scale_id.values()),
            'name': list(scale_id.keys())
        }).set_index('id')

        dfs['scale'] = df_scale

        # creating territory_type dataframe
        df_territory_type = pd.DataFrame({
            'id': list(territory_type_id.values()),
            'name': list(territory_type_id.keys())
        }).set_index('id')

        dfs['territory_type'] = df_territory_type

        df_territory.set_index('id', inplace=True)
        dfs['territory'] = df_territory


    # ## SQL Migration

    exception_type_field = {
        'run_date': 'DATE'
    }


    dsn = 'postgresql://%(USER)s:%(PASSWORD)s@%(HOST)s/%(NAME)s'
    engine = sqla.create_engine(dsn % DATABASE)

    for k, df in dfs.items():
        k_new = k.replace('-', '_')

        print('Migrating %s ...' % k_new)
        df.to_sql(
            k_new, engine, if_exists='replace',
            chunksize=2048
        )


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Update datafiles and DB.\n",
                                     formatter_class=RawDescriptionHelpFormatter)
    parser.add_argument('-F', '--Force', help='Force download', action='store_true')
    parser.add_argument('-b', '--basic', help='Update base tables', action='store_true')
    parser.add_argument('-d', '--database', help='Update database', action='store_true')

    args = parser.parse_args()
    update_data_files(force=args.Force)
    if args.database:
        migrate_from_csv_to_psql(basic_tables=args.basic)

