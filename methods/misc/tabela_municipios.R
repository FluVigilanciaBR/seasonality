library(foreign)
library(tidyverse)

data_folder <- '../data/'
df <- read.dbf(paste0(data_folder, 'base_territorial/tb_municip.dbf'), as.is=T) %>%
  mutate(CO_MUNICIP = as.character(CO_MUNICIP))
Encoding(df$DS_NOME) <- 'latin1'
Encoding(df$DS_OBSERV) <- 'latin1'
df.uf <- read.dbf(paste0(data_folder, 'base_territorial/tb_uf.dbf'), as.is=T) %>%
  select(CO_UF, DS_SIGLA) %>%
  dplyr::rename(DS_UF_SIGLA = DS_SIGLA)
df <- df %>%
  left_join(df.uf)

reg <- 'macsaud'
df.reg <- read.dbf(paste0(data_folder, 'base_territorial/rl_municip_', reg, '.dbf')) %>%
  mutate(CO_MUNICIP = as.character(CO_MUNICIP),
         CO_MACSAUD = as.character(CO_MACSAUD))
dftbl <- read.dbf(paste0(data_folder, 'base_territorial/tb_', reg, '.dbf'), as.is=T) %>%
  mutate(CO_MACSAUD = as.character(CO_MACSAUD)) %>%
  rename(DS_ABREV_macsaud = DS_ABREV)
df.reg <- df.reg %>%
  left_join(dftbl)
df <- df %>%
  left_join(df.reg, by='CO_MUNICIP', suffix = c('_municip', paste0('_', reg)))

reg <- 'regmetr'
df.reg <- read.dbf(paste0(data_folder, 'base_territorial/rl_municip_', reg, '.dbf')) %>%
  mutate(CO_MUNICIP = as.character(CO_MUNICIP),
         CO_REGMETR = as.character(CO_REGMETR))
dftbl <- read.dbf(paste0(data_folder, 'base_territorial/tb_', reg, '.dbf'), as.is=T) %>%
  mutate(CO_REGMETR = as.character(CO_REGMETR)) %>%
  rename(CO_STATUS_regmetr = CO_STATUS,
         CO_TIPO_regmetr = CO_TIPO,
         DS_NOME_regmetr = DS_NOME,
         DS_NOMEPAD_regmetr = DS_NOMEPAD,
         DS_ABREV_regmetr = DS_ABREV,
         NU_ORDEM_regmetr = NU_ORDEM)
Encoding(dftbl$DS_NOME_regmetr) <- 'latin1'
Encoding(dftbl$DS_ABREV_regmetr) <- 'latin1'
df.reg <- df.reg %>%
  left_join(dftbl, by='CO_REGMETR')
df <- df %>%
  left_join(df.reg, by='CO_MUNICIP', suffix = c('_municip', paste0('_', reg)))

write.csv(df, paste0(data_folder, 'municip_macsaud_regmetr.csv'), row.names=F)