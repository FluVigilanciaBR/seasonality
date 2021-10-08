pred.ufs <- readRDS('ufs_current.rds')
pred.capitais <- readRDS('capitais_current.rds')
pred.macros <- readRDS('macros_current.rds')
require(tidyverse)

# UFs -----
# UF transmissão -----
pred.macros %>%
  filter(Date == max(Date)) %>%
  group_by(DS_UF_SIGLA) %>%
  summarise(nivel = max(nivel)) %>%
  group_by(nivel) %>%
  tally()

pred.macros %>%
  filter(Date == max(Date)) %>%
  group_by(nivel) %>%
  tally()

pred.macros %>%
  filter(Date == max(Date)) %>%
  group_by(DS_UF_SIGLA) %>%
  summarise(nivel = max(nivel)) %>%
  ungroup() %>%
  arrange(nivel, DS_UF_SIGLA) %>%
  View()

# UF: tendencias -----
pred.ufs %>%
  filter(grupo_jur == 0, Date >= max(Date) - 5, CO_UF != 0) %>%
  group_by(CO_UF) %>%
  summarise(CO_UF = unique(CO_UF), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.ufs %>% filter(grupo_jur == 0), by='CO_UF') %>%
  filter((tendencia.6s > 0 | tendencia.3s > 0),
           Date == max(Date)) %>%
  arrange(DS_UF_SIGLA) %>%
  select(Date, total.cresc, tendencia.6s, tendencia.3s, DS_UF_SIGLA, CO_UF)

pred.ufs %>%
  filter(grupo_jur == 0, CO_UF != 0) %>%
  group_by(CO_UF) %>%
  summarise(CO_UF = unique(CO_UF), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.ufs %>% filter(grupo_jur == 0), by='CO_UF') %>%
  filter((tendencia.6s < 0) &
           Date == max(Date)) %>%
  arrange(DS_UF_SIGLA) %>%
  select(Date, total.cresc, tendencia.6s, tendencia.3s, DS_UF_SIGLA, CO_UF)

pred.ufs %>%
  filter(grupo_jur == 0, Date >= max(Date) - 5, CO_UF != 0) %>%
  group_by(CO_UF) %>%
  summarise(CO_UF = unique(CO_UF), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.ufs %>% filter(grupo_jur == 0), by='CO_UF') %>%
  filter((tendencia.6s > 0 | tendencia.3s > 0) &
           Date == max(Date)) %>%
  arrange(-tendencia.6s, DS_UF_SIGLA) %>%
  select(Date, total.cresc, tendencia.6s, tendencia.3s, DS_UF_SIGLA, CO_UF)

pred.ufs %>%
  filter(grupo_jur == 0, Date >= max(Date) - 5, CO_UF != 0) %>%
  group_by(CO_UF) %>%
  summarise(CO_UF = unique(CO_UF), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.ufs %>% filter(grupo_jur == 0), by='CO_UF') %>%
  filter((tendencia.6s == 0 & tendencia.3s == 0) &
           Date == max(Date)) %>%
  arrange(-tendencia.6s, DS_UF_SIGLA) %>%
  select(Date, total.cresc, tendencia.6s, tendencia.3s, DS_UF_SIGLA, CO_UF)

# Capitais -----
# Capitais: tendencias -----
pred.capitais %>%
  filter(grupo_jur == 0, Date >= max(Date) - 5) %>%
  group_by(CO_MUN_RES) %>%
  summarise(CO_MUN_RES = unique(CO_MUN_RES), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.capitais %>% filter(grupo_jur == 0), by='CO_MUN_RES') %>%
  filter((tendencia.6s > 0 | tendencia.3s > 0) &
           Date == max(Date)) %>%
  arrange(CO_MUN_RES_nome) %>%
  select(Date, total.cresc, tendencia.6s, tendencia.3s, CO_MUN_RES_nome, DS_UF_SIGLA, CO_UF)

pred.capitais %>%
  filter(grupo_jur == 0) %>%
  group_by(CO_MUN_RES) %>%
  summarise(CO_MUN_RES = unique(CO_MUN_RES), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.capitais %>% filter(grupo_jur == 0), by='CO_MUN_RES') %>%
  filter((tendencia.6s < 0) &
           Date == max(Date)) %>%
  arrange(CO_MUN_RES_nome) %>%
  select(Date, total.cresc, tendencia.6s, tendencia.3s, CO_MUN_RES_nome, DS_UF_SIGLA, CO_UF)

pred.capitais %>%
  filter(grupo_jur == 0, Date >= max(Date) - 5) %>%
  group_by(CO_MUN_RES) %>%
  summarise(CO_MUN_RES = unique(CO_MUN_RES), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.capitais %>%
              filter(grupo_jur == 0), by='CO_MUN_RES') %>%
  filter((tendencia.6s > 0 | tendencia.3s > 0) &
           Date == max(Date)) %>%
  arrange(-tendencia.6s, CO_MUN_RES_nome) %>%
  select(Date, total.cresc, tendencia.6s, tendencia.3s, CO_MUN_RES_nome, DS_UF_SIGLA, CO_UF)

pred.capitais %>%
  filter(grupo_jur == 0, Date >= max(Date) - 5) %>%
  group_by(CO_MUN_RES) %>%
  summarise(CO_MUN_RES = unique(CO_MUN_RES), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.capitais %>%
              filter(grupo_jur == 0), by='CO_MUN_RES') %>%
  filter((tendencia.6s == 0 & tendencia.3s == 0) &
           Date == max(Date)) %>%
  arrange(-tendencia.6s, CO_MUN_RES_nome) %>%
  select(Date, total.cresc, tendencia.6s, tendencia.3s, CO_MUN_RES_nome, DS_UF_SIGLA, CO_UF)

# Nivel de transmissao ------
tblCADMUN <- read.csv("../data/municip_macsaud_regmetr.csv", stringsAsFactors = F) %>%
  filter(IN_CAPITAL %in% c('E', 'F')) %>%
  left_join(pred.macros %>% select(CO_MACSAUD, Date, nivel) %>% filter(Date == max(Date)),
            by='CO_MACSAUD')

tblCADMUN %>%
  filter(Date == max(Date)) %>%
  group_by(nivel) %>%
  tally()
tblCADMUN %>%
  filter(Date == max(Date)) %>%
  select(nivel, DS_NOME_municip, DS_UF_SIGLA) %>%
  arrange(nivel, DS_NOME_municip)


# Macros -----
pred.macros %>%
  filter(Date >= max(Date) - 5) %>%
  group_by(CO_MACSAUD) %>%
  summarise(CO_MACSAUD = unique(CO_MACSAUD), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.macros, by='CO_MACSAUD') %>%
  filter((tendencia.6s > 0 | tendencia.3s > 0) &
           Date == max(Date)) %>%
  arrange(DS_UF_SIGLA) %>%
  select(CO_UF, DS_UF_SIGLA) %>%
  distinct()

pred.macros %>%
  filter(Date == max(Date)) %>%
  group_by(nivel) %>%
  tally()

pred.macros %>%
  filter(Date >= max(Date) - 5) %>%
  group_by(CO_MACSAUD) %>%
  summarise(CO_MACSAUD = unique(CO_MACSAUD), total.cresc = mean(tendencia.6s > 0, na.rm=T)*6) %>%
  left_join(pred.macros, by='CO_MACSAUD') %>%
  filter((tendencia.6s > 0 | tendencia.3s > 0) &
           Date == max(Date)) %>%
  arrange(DS_UF_SIGLA, -tendencia.6s, DS_NOMEPAD_macsaud) %>%
  select(Date, total.cresc, tendencia.6s, tendencia.3s, DS_NOMEPAD_macsaud, DS_UF_SIGLA, CO_UF) %>%
  View()
