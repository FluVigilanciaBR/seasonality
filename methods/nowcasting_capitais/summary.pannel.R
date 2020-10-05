pred.capitais <- readRDS('capitais_2020_39.rds')
pred.macros <- readRDS('macros_2020_39.rds')
require(tidyverse)

pred.capitais %>%
  group_by(CO_MUN_RES) %>%
  summarise(CO_MUN_RES = unique(CO_MUN_RES), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.capitais, by='CO_MUN_RES') %>%
  filter((tendencia.6s > 0 | tendencia.3s > 0) &
           Date == max(Date)) %>%
  arrange(CO_MUN_RES_nome) %>%
  select(Date, total.cresc, tendencia.6s, tendencia.3s, CO_MUN_RES_nome, DS_UF_SIGLA, CO_UF)

pred.macros %>%
  group_by(CO_MACSAUD) %>%
  summarise(CO_MACSAUD = unique(CO_MACSAUD), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.macros, by='CO_MACSAUD') %>%
  filter((tendencia.6s > 0 | tendencia.3s > 0) &
           Date == max(Date)) %>%
  arrange(DS_UF_SIGLA, DS_NOMEPAD_macsaud) %>%
  select(Date, total.cresc, tendencia.6s, tendencia.3s, DS_NOMEPAD_macsaud, DS_UF_SIGLA, CO_UF)
