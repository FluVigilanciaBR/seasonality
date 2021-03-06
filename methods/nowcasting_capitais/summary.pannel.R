pred.capitais <- readRDS('capitais_current.rds')
pred.macros <- readRDS('macros_current.rds')
require(tidyverse)

pred.capitais %>%
  filter(grupo_jur == 0) %>%
  group_by(CO_MUN_RES) %>%
  summarise(CO_MUN_RES = unique(CO_MUN_RES), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.capitais %>% filter(grupo_jur == 0), by='CO_MUN_RES') %>%
  filter((tendencia.6s > 0) &
           Date == max(Date)) %>%
  arrange(-tendencia.6s, CO_MUN_RES_nome) %>%
  select(Date, total.cresc, tendencia.6s, tendencia.3s, CO_MUN_RES_nome, DS_UF_SIGLA, CO_UF)


pred.capitais %>%
  filter(grupo_jur == 0) %>%
  group_by(CO_MUN_RES) %>%
  summarise(CO_MUN_RES = unique(CO_MUN_RES), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.capitais %>%
              filter(grupo_jur == 0), by='CO_MUN_RES') %>%
  filter((tendencia.6s > 0 | tendencia.3s > 0) &
           Date == max(Date)) %>%
  arrange(-tendencia.6s, CO_MUN_RES_nome) %>%
  select(Date, total.cresc, tendencia.6s, tendencia.3s, CO_MUN_RES_nome, DS_UF_SIGLA, CO_UF)


pred.macros %>%
  group_by(CO_MACSAUD) %>%
  summarise(CO_MACSAUD = unique(CO_MACSAUD), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.macros, by='CO_MACSAUD') %>%
  filter((tendencia.6s > 0 | tendencia.3s > 0) &
           Date == max(Date)) %>%
  arrange(DS_UF_SIGLA) %>%
  select(CO_UF, DS_UF_SIGLA) %>%
  unique() %>%
  nrow()

pred.macros %>%
  group_by(CO_MACSAUD) %>%
  summarise(CO_MACSAUD = unique(CO_MACSAUD), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.macros, by='CO_MACSAUD') %>%
  filter((tendencia.6s > 0 | tendencia.3s > 0) &
           Date == max(Date)) %>%
  arrange(DS_UF_SIGLA, -tendencia.6s, DS_NOMEPAD_macsaud) %>%
  select(Date, total.cresc, tendencia.6s, tendencia.3s, DS_NOMEPAD_macsaud, DS_UF_SIGLA, CO_UF) %>%
  View()
