require(tidyverse)
require(vroom)
require(lubridate)

data.cleanup <- function(df){
  df %>%
    mutate(DT_SIN_PRI = as.Date(DT_SIN_PRI),
           DT_NOTIFIC = as.Date(DT_NOTIFIC, format='%m/%d/%Y'),
           DT_DIGITA = as.Date(DT_DIGITA, format='%m/%d/%Y'),
           DT_NASC = as.Date(DT_NASC, format='%m/%d/%Y'),
           idade_em_anos = case_when(
             TP_IDADE == '3' ~ as.integer(NU_IDADE_N),
             TRUE ~ as.integer(0)
           )) %>%
    filter(DT_SIN_PRI >= '2021-10-17')
}

tblCADMUN <- read.csv("~/codes/FluVigilanciaBR/seasonality/methods/data/municip_macsaud_regmetr.csv", stringsAsFactors = F)

dadosBR.se42 <- vroom("~/ownCloud/Fiocruz/Influenza/Dados-MS/email/processed/2021-10-25_SRAG_20211025.zip", delim=';', col_types = cols(
  'sg_uf' = col_character(),
  'sg_uf_not' = col_character(),
  'nu_notific' = col_character(),
  'co_mun_not' = col_double(),
  'dt_notific' = col_character(),
  'dt_digita' =col_character(),
  'dt_sin_pri' = col_character(),
  'dt_nasc' = col_character(),
  'hospital' = col_integer(),
  'evolucao' = col_integer(),
  'nu_idade_n' = col_character(),
  'tp_idade' = col_character(),
  'classi_fin' = col_integer(),
  'pcr_sars2' = col_integer(),
  'pcr_vsr' = col_integer(),
  'an_sars2' = col_integer(),
  'an_vsr' = col_integer(),
  .default = col_character()
)) %>%
  rename_all(toupper) %>%
  filter(SG_UF_NOT=='RS') %>%
  select(SG_UF,
         SG_UF_NOT,
         NU_NOTIFIC,
         CO_MUN_NOT,
         DT_SIN_PRI,
         DT_NOTIFIC,
         DT_DIGITA,
         DT_NASC,
         HOSPITAL,
         EVOLUCAO,
         NU_IDADE_N,
         TP_IDADE,
         CLASSI_FIN,
         PCR_SARS2,
         PCR_VSR,
         AN_SARS2,
         AN_VSR) %>%
  distinct(NU_NOTIFIC, CO_MUN_NOT, DT_NOTIFIC, .keep_all=T)
gc()

dadosBR.se42 <- data.cleanup(dadosBR.se42)
gc()

dadosBR.se44 <- vroom("~/ownCloud/Fiocruz/Influenza/Dados-MS/email/processed/2021-11-09_SRAG_20211108.zip", delim=';', col_types = cols(
  'sg_uf' = col_character(),
  'sg_uf_not' = col_character(),
  'nu_notific' = col_character(),
  'co_mun_not' = col_double(),
  'dt_notific' = col_character(),
  'dt_digita' =col_character(),
  'dt_sin_pri' = col_character(),
  'dt_nasc' = col_character(),
  'hospital' = col_integer(),
  'evolucao' = col_integer(),
  'nu_idade_n' = col_character(),
  'tp_idade' = col_character(),
  'classi_fin' = col_integer(),
  'pcr_sars2' = col_integer(),
  'pcr_vsr' = col_integer(),
  'an_sars2' = col_integer(),
  'an_vsr' = col_integer(),
  .default = col_character()
)) %>%
  rename_all(toupper) %>%
  filter(SG_UF_NOT=='RS') %>%
  select(SG_UF,
         SG_UF_NOT,
         NU_NOTIFIC,
         CO_MUN_NOT,
         DT_SIN_PRI,
         DT_NOTIFIC,
         DT_DIGITA,
         DT_NASC,
         HOSPITAL,
         EVOLUCAO,
         NU_IDADE_N,
         TP_IDADE,
         CLASSI_FIN,
         PCR_SARS2,
         PCR_VSR,
         AN_SARS2,
         AN_VSR) %>%
  distinct(NU_NOTIFIC, CO_MUN_NOT, DT_NOTIFIC, .keep_all=T)
gc()

dadosBR.se44 <- data.cleanup(dadosBR.se44)
gc()
