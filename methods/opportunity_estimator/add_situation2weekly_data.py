# coding:utf8
__author__ = 'Marcelo Ferreira da Costa Gomes'

import pandas as pd


df_est = pd.read_csv('../clean_data/current_estimated_values.csv', low_memory=False, encoding='utf-8')
df_age = pd.read_csv('../clean_data/clean_data_epiweek-weekly-incidence.csv', low_memory=False, encoding='utf-8')
df_est_simp = df_est[['UF', 'epiyear', 'epiweek', 'Situation']]
df_age_full = df_age.merge(df_est_simp, on=['UF', 'epiyear', 'epiweek'], how='left')
df_age_full.dropna(axis=0, inplace=True)
df_age_full.loc[df_age_full.Situation != 'stable', 'Situation'] = 'unknown'

df_age_full.to_csv('../clean_data/clean_data_epiweek-weekly-incidence_w_situation.csv', index=False, encoding='utf-8')